package build

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/transpiler3/c/emit"
	"mochi/transpiler3/c/lower"
	"mochi/transpiler3/c/runtime"
	"mochi/types"
)

// Driver is the entry-point for source-to-binary builds.
// Callers construct one Driver per build and call Build.
//
// Phase 1 surface: parse, type-check, lower, emit, compile +
// link. Sub-phase 1.1 adds CLI flag wiring (--out, --emit=c).
// Sub-phase 1.2 adds the .mochi/cache content-addressed
// layer. Sub-phase 1.3 adds the vendored zig fallback for
// missing host cc.
type Driver struct {
	// CC overrides host-cc discovery. Empty means: discover via
	// $CC, then "cc", "clang", "gcc" on PATH; then (Phase 1.3)
	// fall back to the vendored zig.
	CC string

	// KeepEmit, when true, leaves the emitted C source next to
	// the produced binary as <out>.c. Phase 1.1 wires this from
	// --emit=c on the CLI.
	KeepEmit bool

	// EmittedCPath, set by Build on success when KeepEmit=true,
	// is the absolute path the emitted C source was written to.
	EmittedCPath string
}

// Build is the source-to-binary entry point. src is a Mochi
// source file; out is the desired binary path; target is a
// triple (empty for host) and profile is one of "debug",
// "release", "" (treated as "debug" for Phase 1).
//
// Phase 1 ignores target (host triple only; Phase 11 wires it)
// and ignores profile (debug-equivalent; Phase 1.2 wires it
// into the cache key).
func (d *Driver) Build(src, out, target, profile string) error {
	if src == "" {
		return errors.New("transpiler3/c/build: source path is required")
	}
	if out == "" {
		return errors.New("transpiler3/c/build: output path is required")
	}

	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: parse %s: %w", src, err)
	}
	if errs := types.Check(prog, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("transpiler3/c/build: type check %s: %w", src, errs[0])
	}
	ir, err := lower.Lower(prog)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: lower %s: %w", src, err)
	}
	csrc, err := emit.Emit(ir)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: emit %s: %w", src, err)
	}

	workDir, err := os.MkdirTemp("", "mochi-aot-")
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdtemp: %w", err)
	}
	defer func() {
		if !d.KeepEmit {
			_ = os.RemoveAll(workDir)
		}
	}()

	if err := os.MkdirAll(filepath.Join(workDir, "include", "mochi"), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir include: %w", err)
	}
	if err := os.MkdirAll(filepath.Join(workDir, "src"), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir src: %w", err)
	}
	if err := writeRuntimeFiles(workDir); err != nil {
		return fmt.Errorf("transpiler3/c/build: stage runtime: %w", err)
	}
	genPath := filepath.Join(workDir, "main.c")
	if err := os.WriteFile(genPath, []byte(csrc), 0o644); err != nil {
		return fmt.Errorf("transpiler3/c/build: write gen: %w", err)
	}

	cc, err := d.resolveCC()
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: %w", err)
	}

	absOut, err := filepath.Abs(out)
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: abs %s: %w", out, err)
	}
	if err := os.MkdirAll(filepath.Dir(absOut), 0o755); err != nil {
		return fmt.Errorf("transpiler3/c/build: mkdir out: %w", err)
	}

	args := []string{
		"-std=c2x",
		"-Wall", "-Wextra", "-pedantic",
		"-I", filepath.Join(workDir, "include"),
		"-o", absOut,
		genPath,
		filepath.Join(workDir, "src", "print.c"),
	}
	cmd := exec.Command(cc, args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("transpiler3/c/build: cc %s %s: %w\n%s",
			cc, strings.Join(args, " "), err, output)
	}

	if d.KeepEmit {
		final := absOut + ".c"
		if err := os.Rename(genPath, final); err != nil {
			if data, rerr := os.ReadFile(genPath); rerr == nil {
				_ = os.WriteFile(final, data, 0o644)
			}
		}
		d.EmittedCPath = final
	}
	return nil
}

func writeRuntimeFiles(workDir string) error {
	return fs.WalkDir(runtime.Files, ".", func(path string, e fs.DirEntry, err error) error {
		if err != nil || e.IsDir() {
			return err
		}
		data, err := runtime.Files.ReadFile(path)
		if err != nil {
			return err
		}
		dst := filepath.Join(workDir, path)
		if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
			return err
		}
		return os.WriteFile(dst, data, 0o644)
	})
}

// resolveCC looks up the C compiler to invoke. Phase 1.0 walks
// the standard candidates ($CC, then cc, clang, gcc on PATH).
// Phase 1.3 appends the vendored zig fallback.
func (d *Driver) resolveCC() (string, error) {
	if d.CC != "" {
		return d.CC, nil
	}
	if env := strings.TrimSpace(os.Getenv("CC")); env != "" {
		return env, nil
	}
	for _, name := range []string{"cc", "clang", "gcc"} {
		if path, err := exec.LookPath(name); err == nil {
			return path, nil
		}
	}
	return "", errors.New("no C compiler found: set $CC or install cc/clang/gcc on PATH (Phase 1.3 will add a vendored zig fallback)")
}
