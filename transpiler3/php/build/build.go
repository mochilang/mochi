// Package build is the PHP transpiler pipeline driver.
// Entry point: Driver.Build(src, outDir string, target Target) (string, error).
//
// Phase 0 ships the skeleton: parse → typecheck → aotir lower →
// colour → ptree lower → emit. Running the emitted PHP under
// the host's `php` binary is enabled for TargetPhpRun; otherwise
// we just write the .php file. Later phases plug in Composer,
// PHPStan, Psalm, php-cs-fixer, and php main.php with the
// generated bootstrap.
package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/php/colour"
	"mochi/transpiler3/php/emit"
	"mochi/transpiler3/php/lower"
	"mochi/types"
)

// Target selects the PHP build output format.
type Target int

const (
	// TargetPhpSource writes the generated .php source to outDir and
	// returns its path. Useful for golden-file tests.
	TargetPhpSource Target = iota
	// TargetPhpRun is identical to TargetPhpSource plus a `php` invocation
	// of the entry file. The returned path is still the .php file; the
	// caller must capture stdout separately if it wants the program output.
	TargetPhpRun
)

// Driver is the PHP transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/php/ location.
	CacheDir string
	// NoCache disables the build cache.
	NoCache bool
	// Deterministic is reserved for future SOURCE_DATE_EPOCH wiring
	// and sorted-Phar packaging. Today it is a no-op: the lower +
	// emit pipeline has no time-, random-, or PATH-derived sources
	// of non-determinism, so builds are byte-equal regardless of
	// this flag. TestPhase16NonDeterministicBuildsAlsoMatch is the
	// gate that keeps it that way.
	Deterministic bool

	phpPath string
}

// Build emits the .php source for src into outDir. When target ==
// TargetPhpRun and the host has `php` on PATH, the driver also invokes
// `php main.php` and forwards its stdout/stderr to the caller's
// streams. The returned string is always the path to the emitted .php
// file on disk.
func (d *Driver) Build(src, outDir string, target Target) (string, error) {
	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return "", fmt.Errorf("php build: read %s: %w", src, err)
	}

	ast, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("php build: parse: %w", err)
	}

	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("php build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("php build: aotir lower: %w", err)
	}

	colours := colour.Compute(prog)
	file, err := lower.Lower(prog, colours)
	if err != nil {
		return "", fmt.Errorf("php build: php lower: %w", err)
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	emittedPath, err := emit.Emit(file, outDir, "main")
	if err != nil {
		return "", fmt.Errorf("php build: emit: %w", err)
	}

	if target == TargetPhpRun {
		if d.phpPath == "" {
			p, err := resolvePhp()
			if err != nil {
				return emittedPath, fmt.Errorf("php build: %w", err)
			}
			d.phpPath = p
		}
		runCmd := exec.Command(d.phpPath, emittedPath)
		runCmd.Stdout = os.Stdout
		runCmd.Stderr = os.Stderr
		if err := runCmd.Run(); err != nil {
			return emittedPath, fmt.Errorf("php build: php run: %w", err)
		}
	}

	// Touch unused fields so future phases that consume them keep
	// the compiler honest about the field set we publish today.
	_ = srcBytes
	_ = d.cacheKey
	_ = d.effectiveCacheDir
	_ = copyFile
	_ = sha256.New
	_ = io.Copy
	return emittedPath, nil
}

// resolvePhp finds the php binary or returns an error. Phase 0 honours
// PHP_PATH first (CI sets it via shivammathur/setup-php@v2), then walks
// well-known paths, then falls back to exec.LookPath.
func resolvePhp() (string, error) {
	if p := os.Getenv("PHP_PATH"); p != "" {
		if info, err := os.Stat(p); err == nil && info.IsDir() {
			p = filepath.Join(p, "php")
		}
		return p, nil
	}
	for _, candidate := range []string{"/usr/bin/php", "/usr/local/bin/php", "/opt/homebrew/bin/php"} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("php")
	if err != nil {
		return "", fmt.Errorf("php not found on PATH (set PHP_PATH or add php to PATH): %w", err)
	}
	return p, nil
}

// effectiveCacheDir returns the build cache directory.
func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "php")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "php")
}

// cacheKey computes a SHA-256 cache key from source bytes and php
// version. Currently unused: every build invokes the pipeline from
// scratch. The helper is reserved for a future cache integration
// (NoCache and CacheDir on Driver are part of the same forward-compat
// surface). The `_ = d.cacheKey` line in Build keeps the symbol live
// so removing it across the codebase is a single-spot decision.
func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	h.Write([]byte(d.phpPath))
	if d.Deterministic {
		h.Write([]byte{1})
	} else {
		h.Write([]byte{0})
	}
	return fmt.Sprintf("%x", h.Sum(nil))
}

// runtimeSourceDir returns the absolute path to the PHP runtime
// (transpiler3/php/runtime/) so the build driver can copy Composer
// metadata into a sandbox once Phase 15 is wired.
func runtimeSourceDir() string {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return ""
	}
	dir := filepath.Dir(thisFile)
	phpDir := filepath.Dir(dir)
	p := filepath.Join(phpDir, "runtime")
	if _, err := os.Stat(p); err != nil {
		return ""
	}
	return p
}

// copyFile copies src to dst, creating dst's parent directories as
// needed. Phase 15 uses this when staging the Composer package.
func copyFile(dst, src string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}

// repoRootForBuild walks up from this source file until it finds a
// go.mod. Tests use this to find the fixture directory regardless of
// the working directory the test runner picked.
func repoRootForBuild(t interface {
	Helper()
	Fatalf(string, ...any)
}) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller(0) failed")
	}
	dir := filepath.Dir(thisFile)
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("go.mod not found walking up from %s", thisFile)
		}
		dir = parent
	}
}
