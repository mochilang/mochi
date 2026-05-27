package build

import (
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

	// Prefer $DOTNET_ROOT if set.
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

	// Output: "8.0.204\n" or "9.0.0-rc.1\n"
	versionStr := strings.TrimSpace(string(out))
	// Strip pre-release suffix: "9.0.0-rc.1" -> "9.0.0"
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
	tc      *Toolchain
}

// Build compiles src to the given target artefact at out.
// In Phase 0 this only performs parse + typecheck; full pipeline comes in Phase 1.
func (d *Driver) Build(src, out string, target Target) error {
	// Resolve toolchain on first call.
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}

	// Parse.
	ast, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("dotnet build: parse: %w", err)
	}

	// Type-check.
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("dotnet build: typecheck: %w", errs[0])
	}

	// Lower to aotir.
	prog, err := clower.Lower(ast)
	if err != nil {
		return fmt.Errorf("dotnet build: aotir lower: %w", err)
	}

	// Colour pass (all Blue in Phase 0).
	colours := colour.Analyse(prog)

	// Lower to csharpsrc (stub in Phase 0 - returns empty CU).
	className := lower.ClassName(src)
	_, err = lower.Lower(prog, colours, className)
	if err != nil {
		return fmt.Errorf("dotnet build: dotnet lower: %w", err)
	}

	// Phase 0: no actual .NET output. Full emit pipeline comes in Phase 1.
	_ = out
	_ = target
	return nil
}

// repoRootForBuild returns the absolute path to the repo root by walking up
// from this Go source file (build.go) until it finds go.mod.
// It uses runtime.Caller(0) so the path is correct regardless of the working
// directory at test time.
func repoRootForBuild(t interface {
	Helper()
	Fatalf(string, ...any)
}) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller(0) failed")
	}
	// thisFile is .../transpiler3/dotnet/build/build.go; walk up to find go.mod.
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
