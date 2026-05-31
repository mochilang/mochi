// Package build implements the MEP-69 phase 7 Swift build driver. It invokes
// swift build to compile a Swift package into a static library and collects
// the resulting .a files and linker flags for use by the Mochi CGo bridge.
package build

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Driver is the Swift build driver.
type Driver struct {
	// WorkDir is the directory where build artifacts are placed.
	WorkDir string
	// SwiftBin is the path to the swift binary. If empty, "swift" is looked up
	// on PATH.
	SwiftBin string
	// SDKPath is an optional explicit SDK path (passed as -sdk to swiftc).
	SDKPath string
}

// BuildSpec describes a Swift package build request.
type BuildSpec struct {
	// PackageURL is the URL or local path of the Swift package to build.
	PackageURL string
	// Version is the version tag to build.
	Version string
	// Platforms is a list of target platform identifiers (e.g. "macos-arm64").
	Platforms []string
	// CacheDir is the directory where downloaded source archives are cached.
	CacheDir string
}

// PackageLink is the result of a successful build. It provides the paths and
// flags needed to link the compiled library into a CGo project.
type PackageLink struct {
	// PackageID is the identifier of the built package.
	PackageID string
	// StaticLibs maps platform identifier to the path of the compiled .a file.
	StaticLibs map[string]string
	// HeaderDir is the directory containing generated Objective-C bridge headers.
	HeaderDir string
	// LinkerFlags are the macOS linker flags needed to link the library.
	LinkerFlags []string
}

// swiftBin returns the swift binary path, falling back to PATH lookup.
func (d *Driver) swiftBin() (string, error) {
	if d.SwiftBin != "" {
		return d.SwiftBin, nil
	}
	return exec.LookPath("swift")
}

// Build compiles the Swift package described by spec. Returns a PackageLink on
// success. If swift is not on PATH the error wraps exec.ErrNotFound.
func (d *Driver) Build(ctx context.Context, spec BuildSpec) (*PackageLink, error) {
	// Validate spec
	if spec.PackageURL == "" {
		return nil, fmt.Errorf("build: PackageURL is required")
	}

	swiftBin, err := d.swiftBin()
	if err != nil {
		return nil, fmt.Errorf("build: swift not found: %w", err)
	}

	// Ensure work directory exists
	workDir := d.WorkDir
	if workDir == "" {
		workDir = os.TempDir()
	}
	pkgDir := filepath.Join(workDir, sanitizeName(spec.PackageURL)+"@"+sanitizeName(spec.Version))
	if err := os.MkdirAll(pkgDir, 0o755); err != nil {
		return nil, fmt.Errorf("build: mkdir workdir: %w", err)
	}

	// Run swift build -c release
	args := []string{"build", "-c", "release"}
	if d.SDKPath != "" {
		args = append(args, "-Xswiftc", "-sdk", "-Xswiftc", d.SDKPath)
	}

	cmd := exec.CommandContext(ctx, swiftBin, args...)
	cmd.Dir = pkgDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("build: swift build failed: %w\noutput:\n%s", err, string(out))
	}

	// Collect .a files from .build/release/
	buildDir := filepath.Join(pkgDir, ".build", "release")
	staticLibs, err := collectStaticLibs(buildDir)
	if err != nil {
		return nil, fmt.Errorf("build: collect .a files: %w", err)
	}

	// Determine package ID from URL
	pkgID := filepath.Base(spec.PackageURL)
	pkgID = strings.TrimSuffix(pkgID, ".git")

	return &PackageLink{
		PackageID:  pkgID,
		StaticLibs: staticLibs,
		HeaderDir:  filepath.Join(pkgDir, ".build", "release"),
		LinkerFlags: []string{
			"-framework", "Foundation",
			"-lc++",
		},
	}, nil
}

// collectStaticLibs finds all .a files in dir, keyed by the platform derived
// from the directory structure (default: "macos").
func collectStaticLibs(dir string) (map[string]string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return map[string]string{}, nil
		}
		return nil, err
	}

	libs := make(map[string]string)
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".a") {
			libs["macos"] = filepath.Join(dir, e.Name())
			break // take the first .a
		}
	}
	return libs, nil
}

// sanitizeName makes a string safe for use as a directory component.
func sanitizeName(s string) string {
	replacer := strings.NewReplacer(
		"/", "_",
		":", "_",
		"@", "_",
		".", "_",
	)
	return replacer.Replace(s)
}
