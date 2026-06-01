// Package macroexpand runs `swift build` with macro plugins active before
// .swiftinterface extraction, so macro-expanded declarations are visible.
package macroexpand

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// ExpandResult holds the paths produced by a macro expansion run.
type ExpandResult struct {
	// PackageDir is the root of the Swift package.
	PackageDir string
	// BuildDir is the .build directory produced by swift build.
	BuildDir string
	// Interfaces is the list of .swiftinterface files discovered after build.
	Interfaces []string
}

// Config controls the macro expansion build.
type Config struct {
	// SwiftBin is the path to the `swift` binary.
	SwiftBin string
	// Configuration is "debug" or "release".
	Configuration string
	// ExtraArgs are passed verbatim to `swift build`.
	ExtraArgs []string
}

// Run invokes `swift build` in packageDir with macro plugins active,
// then locates the produced .swiftinterface files in the .build directory.
func Run(ctx context.Context, packageDir string, cfg Config) (*ExpandResult, error) {
	if packageDir == "" {
		return nil, fmt.Errorf("macroexpand: packageDir is empty")
	}
	if _, err := os.Stat(packageDir); err != nil {
		return nil, fmt.Errorf("macroexpand: packageDir %s: %w", packageDir, err)
	}

	swiftBin := cfg.SwiftBin
	if swiftBin == "" {
		var err error
		swiftBin, err = exec.LookPath("swift")
		if err != nil {
			return nil, fmt.Errorf("macroexpand: swift not on PATH: %w", err)
		}
	}

	configuration := cfg.Configuration
	if configuration == "" {
		configuration = "release"
	}

	args := append([]string{"build", "-c", configuration}, cfg.ExtraArgs...)
	cmd := exec.CommandContext(ctx, swiftBin, args...)
	cmd.Dir = packageDir
	cmd.Env = os.Environ()

	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("macroexpand: swift build: %w\n%s", err, out)
	}

	buildDir := filepath.Join(packageDir, ".build")
	interfaces, err := findSwiftInterfaces(buildDir)
	if err != nil {
		interfaces = nil
	}

	return &ExpandResult{
		PackageDir: packageDir,
		BuildDir:   buildDir,
		Interfaces: interfaces,
	}, nil
}

// findSwiftInterfaces walks dir recursively and returns all .swiftinterface paths.
func findSwiftInterfaces(dir string) ([]string, error) {
	var out []string
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if !info.IsDir() && filepath.Ext(path) == ".swiftinterface" {
			out = append(out, path)
		}
		return nil
	})
	return out, err
}
