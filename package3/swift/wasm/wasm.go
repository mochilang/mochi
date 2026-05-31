// Package wasm supports consuming Swift packages compiled for
// wasm32-unknown-wasi via the Swift WebAssembly toolchain.
// The wrapper uses the same @_cdecl pattern as the native bridge.
package wasm

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// WasmTarget is the Wasm triple used by the Swift WebAssembly toolchain.
const WasmTarget = "wasm32-unknown-wasi"

// BuildSpec describes a Wasm build of a Swift package.
type BuildSpec struct {
	// PackageDir is the root of the Swift package.
	PackageDir string
	// SwiftWasmBin is the path to the swiftwasm `swift` binary.
	SwiftWasmBin string
	// OutputDir is where the .wasm file should be placed.
	OutputDir string
	// ExtraArgs are passed verbatim to the compiler.
	ExtraArgs []string
}

// BuildResult holds the output of a Wasm build.
type BuildResult struct {
	// WasmPath is the path to the compiled .wasm module.
	WasmPath string
	// TextPath is the path to the .wat text format (if produced).
	TextPath string
}

// Build compiles a Swift package to WebAssembly using the SwiftWasm toolchain.
// Requires the SwiftWasm toolchain to be installed separately.
func Build(ctx context.Context, spec BuildSpec) (*BuildResult, error) {
	if spec.PackageDir == "" {
		return nil, fmt.Errorf("wasm: PackageDir is empty")
	}
	if _, err := os.Stat(spec.PackageDir); err != nil {
		return nil, fmt.Errorf("wasm: PackageDir %s: %w", spec.PackageDir, err)
	}

	swiftBin := spec.SwiftWasmBin
	if swiftBin == "" {
		var err error
		swiftBin, err = exec.LookPath("swift")
		if err != nil {
			return nil, fmt.Errorf("wasm: swift not on PATH: %w", err)
		}
	}

	outDir := spec.OutputDir
	if outDir == "" {
		outDir = filepath.Join(spec.PackageDir, ".build", "wasm32-unknown-wasi", "release")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return nil, fmt.Errorf("wasm: mkdir: %w", err)
	}

	args := []string{
		"build",
		"-c", "release",
		"--triple", WasmTarget,
	}
	args = append(args, spec.ExtraArgs...)

	cmd := exec.CommandContext(ctx, swiftBin, args...)
	cmd.Dir = spec.PackageDir
	cmd.Env = os.Environ()

	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("wasm: swift build: %w\n%s", err, out)
	}

	// Locate the .wasm output.
	wasmPath, err := findWasmOutput(outDir)
	if err != nil {
		return nil, fmt.Errorf("wasm: find output: %w", err)
	}

	return &BuildResult{WasmPath: wasmPath}, nil
}

func findWasmOutput(dir string) (string, error) {
	var found string
	_ = filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if !info.IsDir() && filepath.Ext(path) == ".wasm" {
			found = path
			return filepath.SkipAll
		}
		return nil
	})
	if found == "" {
		return "", fmt.Errorf("no .wasm file found in %s", dir)
	}
	return found, nil
}
