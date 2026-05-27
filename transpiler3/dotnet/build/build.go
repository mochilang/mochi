// Package build is the .NET transpiler pipeline driver.
// Entry point: Driver.Build(src, out string, target Target) error.
package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/dotnet/colour"
	"mochi/transpiler3/dotnet/emit"
	"mochi/transpiler3/dotnet/lower"
	"mochi/types"
)

// Target selects the .NET packaging format.
type Target int

const (
	TargetFxDependent  Target = iota // framework-dependent .NET app (requires runtime on host)
	TargetSelfContained              // self-contained single-directory publish
	TargetAot                        // NativeAOT ahead-of-time compiled binary
	TargetSingleFile                 // self-contained single-file executable
	TargetNuGet                      // NuGet package (.nupkg)
	TargetDotnetSource               // .cs source files only (debug)
)

// Toolchain holds the resolved dotnet binary path and version information.
type Toolchain struct {
	Dotnet string // absolute path to dotnet binary
	Major  int    // SDK major version (e.g. 8)
	Minor  int    // SDK minor version (e.g. 0)
	Patch  int    // SDK patch version (e.g. 204)
	RID    string // runtime identifier, e.g. "linux-x64" (empty until needed)
}

// resolveToolchain finds the dotnet binary via $DOTNET_ROOT or PATH,
// runs `dotnet --version`, and parses the version string.
func resolveToolchain() (*Toolchain, error) {
	var dotnetPath string

	if dr := os.Getenv("DOTNET_ROOT"); dr != "" {
		candidate := filepath.Join(dr, "dotnet")
		if _, err := os.Stat(candidate); err == nil {
			dotnetPath = candidate
		}
	}

	if dotnetPath == "" {
		var err error
		dotnetPath, err = exec.LookPath("dotnet")
		if err != nil {
			return nil, fmt.Errorf("dotnet not found on PATH (set DOTNET_ROOT or add dotnet to PATH): %w", err)
		}
	}

	out, err := exec.Command(dotnetPath, "--version").Output()
	if err != nil {
		return nil, fmt.Errorf("dotnet --version: %w", err)
	}

	versionStr := strings.TrimSpace(string(out))
	if idx := strings.IndexByte(versionStr, '-'); idx >= 0 {
		versionStr = versionStr[:idx]
	}

	parts := strings.SplitN(versionStr, ".", 3)
	if len(parts) < 3 {
		return nil, fmt.Errorf("unexpected dotnet --version output: %q", string(out))
	}

	major, err := strconv.Atoi(parts[0])
	if err != nil {
		return nil, fmt.Errorf("cannot parse dotnet major version from %q", string(out))
	}
	if major < 8 {
		return nil, fmt.Errorf("dotnet SDK 8.0+ required; found %s at %s", strings.TrimSpace(string(out)), dotnetPath)
	}
	minor, err := strconv.Atoi(parts[1])
	if err != nil {
		return nil, fmt.Errorf("cannot parse dotnet minor version from %q", string(out))
	}
	patch, err := strconv.Atoi(parts[2])
	if err != nil {
		return nil, fmt.Errorf("cannot parse dotnet patch version from %q", string(out))
	}

	return &Toolchain{
		Dotnet: dotnetPath,
		Major:  major,
		Minor:  minor,
		Patch:  patch,
	}, nil
}

// Driver is the .NET transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/dotnet/ location.
	CacheDir string
	// NoCache disables the build cache.
	NoCache bool
	// TFM overrides the target framework moniker (e.g. "net8.0").
	// Defaults to net<Major>.0 matching the installed SDK.
	TFM string
	tc  *Toolchain
}

// effectiveTFM returns the target framework moniker to use for builds.
// Priority: d.TFM > TEST_DOTNET_TFM env > net<major>.0 from SDK version.
func (d *Driver) effectiveTFM() string {
	if d.TFM != "" {
		return d.TFM
	}
	if t := os.Getenv("TEST_DOTNET_TFM"); t != "" {
		return t
	}
	if d.tc != nil {
		return fmt.Sprintf("net%d.0", d.tc.Major)
	}
	return "net8.0"
}

// Build compiles src to the given target artefact at out.
// For TargetFxDependent, out is a directory that receives the published artefacts.
// For TargetDotnetSource, out is a directory that receives .cs files.
func (d *Driver) Build(src, out string, target Target) error {
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}

	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("dotnet build: read %s: %w", src, err)
	}

	cacheKey := d.cacheKey(srcBytes)
	if !d.NoCache && target == TargetFxDependent {
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if fi, err := os.Stat(cacheEntry); err == nil && fi.IsDir() {
			return copyDir(out, cacheEntry)
		}
	}

	ast, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("dotnet build: parse: %w", err)
	}

	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("dotnet build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return fmt.Errorf("dotnet build: aotir lower: %w", err)
	}

	colours := colour.Analyse(prog)
	className := lower.ClassName(src)
	cus, err := lower.Lower(prog, colours, className)
	if err != nil {
		return fmt.Errorf("dotnet build: dotnet lower: %w", err)
	}

	workDir, err := os.MkdirTemp("", "mochi-dotnet-*")
	if err != nil {
		return err
	}
	defer os.RemoveAll(workDir)

	srcDir := filepath.Join(workDir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return err
	}

	for _, cu := range cus {
		if _, err := emit.Emit(cu, srcDir); err != nil {
			return fmt.Errorf("dotnet build: emit: %w", err)
		}
	}

	if target == TargetDotnetSource {
		if err := os.MkdirAll(out, 0o755); err != nil {
			return err
		}
		return copyDir(out, srcDir)
	}

	tfm := d.effectiveTFM()
	runtimeProj := runtimeCsprojPath()
	rid := hostRID()

	var csprojContent string
	if target == TargetAot {
		csprojContent = generateAotCsproj(className, tfm, rid, runtimeProj)
	} else {
		csprojContent = generateCsproj(className, tfm, runtimeProj)
	}
	csprojPath := filepath.Join(srcDir, className+".csproj")
	if err := os.WriteFile(csprojPath, []byte(csprojContent), 0o644); err != nil {
		return fmt.Errorf("dotnet build: write csproj: %w", err)
	}

	switch target {
	case TargetFxDependent:
		pubDir := filepath.Join(workDir, "pub")
		if err := os.MkdirAll(pubDir, 0o755); err != nil {
			return err
		}
		if err := packFxDependent(d.tc.Dotnet, srcDir, pubDir, tfm); err != nil {
			return fmt.Errorf("dotnet build: publish: %w", err)
		}
		if !d.NoCache {
			cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
			if err := os.MkdirAll(filepath.Dir(cacheEntry), 0o755); err == nil {
				_ = copyDir(cacheEntry, pubDir)
			}
		}
		if err := os.MkdirAll(out, 0o755); err != nil {
			return err
		}
		return copyDir(out, pubDir)
	case TargetSelfContained:
		return packSelfContained(d.tc.Dotnet, srcDir, out, tfm)
	case TargetAot:
		if err := os.MkdirAll(out, 0o755); err != nil {
			return err
		}
		return packAot(d.tc.Dotnet, srcDir, out, tfm)
	default:
		return fmt.Errorf("dotnet build: unsupported target %d in Phase 1", target)
	}
}

// effectiveCacheDir returns the build cache directory.
func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "dotnet")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "dotnet")
}

// cacheKey computes a SHA-256 cache key from source bytes and SDK version.
func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	if d.tc != nil {
		fmt.Fprintf(h, "%d.%d.%d", d.tc.Major, d.tc.Minor, d.tc.Patch)
	}
	h.Write([]byte(d.effectiveTFM()))
	return fmt.Sprintf("%x", h.Sum(nil))
}

// runtimeCsprojPath returns the absolute path to Mochi.Runtime.csproj.
// Uses runtime.Caller(0) to locate the source tree so tests work
// regardless of the working directory.
func runtimeCsprojPath() string {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return ""
	}
	// thisFile: .../transpiler3/dotnet/build/build.go
	// runtime:  .../transpiler3/dotnet/runtime/Mochi.Runtime/Mochi.Runtime.csproj
	dir := filepath.Dir(thisFile)
	p := filepath.Join(filepath.Dir(dir), "runtime", "Mochi.Runtime", "Mochi.Runtime.csproj")
	if _, err := os.Stat(p); err != nil {
		return ""
	}
	return p
}

// repoRootForBuild returns the absolute repo root path by walking up from this source file.
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

// copyFile copies src to dst, creating dst's parent directories as needed.
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

// copyDir copies all regular files from srcDir into dstDir (non-recursive).
func copyDir(dstDir, srcDir string) error {
	if err := os.MkdirAll(dstDir, 0o755); err != nil {
		return err
	}
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if err := copyFile(filepath.Join(dstDir, e.Name()), filepath.Join(srcDir, e.Name())); err != nil {
			return err
		}
	}
	return nil
}
