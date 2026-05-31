// Package edge validates edge-runtime compatibility and emits Jupyter kernel specs.
// Lands in MEP-72 Phase 17.
package edge

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// ValidationResult carries the outcome of edge compatibility validation.
type ValidationResult struct {
	EdgeCompatible  bool
	BundlePath      string
	SizeBytes       int64
	NodeBuiltinsUsed []string
}

// ValidateEdgeSpec describes an edge validation run.
type ValidateEdgeSpec struct {
	Entrypoint string
	OutDir     string
	EsbuildBin string
}

// ValidateEdge runs esbuild with edge conditions and checks for Node.js built-in usage.
func ValidateEdge(ctx context.Context, spec ValidateEdgeSpec) (ValidationResult, error) {
	if err := os.MkdirAll(spec.OutDir, 0o755); err != nil {
		return ValidationResult{}, fmt.Errorf("edge: mkdir: %w", err)
	}
	outFile := filepath.Join(spec.OutDir, "edge_bundle.js")
	esbuild := spec.EsbuildBin
	if esbuild == "" {
		esbuild = "esbuild"
	}

	args := []string{
		spec.Entrypoint,
		"--bundle",
		"--format=esm",
		"--platform=browser",
		"--conditions=edge-light,worker",
		"--outfile=" + outFile,
	}
	cmd := exec.CommandContext(ctx, esbuild, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return ValidationResult{}, fmt.Errorf("edge: esbuild: %w\n%s", err, out)
	}

	data, err := os.ReadFile(outFile)
	if err != nil {
		return ValidationResult{}, fmt.Errorf("edge: read bundle: %w", err)
	}
	content := string(data)

	builtins := detectNodeBuiltins(content)
	info, _ := os.Stat(outFile)
	var size int64
	if info != nil {
		size = info.Size()
	}

	return ValidationResult{
		EdgeCompatible:  len(builtins) == 0,
		BundlePath:      outFile,
		SizeBytes:       size,
		NodeBuiltinsUsed: builtins,
	}, nil
}

var nodeBuiltinModules = []string{
	"fs", "path", "crypto", "net", "os", "child_process",
	"stream", "buffer", "http", "https", "url", "util",
	"events", "assert", "zlib", "dns", "tls", "vm",
}

func detectNodeBuiltins(content string) []string {
	var found []string
	for _, mod := range nodeBuiltinModules {
		quoted := []string{
			`require("` + mod + `")`,
			`require('` + mod + `')`,
			`from "` + mod + `"`,
			`from '` + mod + `'`,
			`"node:` + mod + `"`,
			`'node:` + mod + `'`,
		}
		for _, q := range quoted {
			if strings.Contains(content, q) {
				found = append(found, mod)
				break
			}
		}
	}
	return found
}

// JupyterKernelSpec is the Jupyter kernel metadata for Mochi/Deno.
type JupyterKernelSpec struct {
	KernelName  string
	DisplayName string
	Language    string
	DenoPath    string
}

// EmitJupyterKernelSpec writes a kernel.json to outDir.
func EmitJupyterKernelSpec(spec JupyterKernelSpec, outDir string) error {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return fmt.Errorf("edge: mkdir kernel spec: %w", err)
	}
	denoBin := spec.DenoPath
	if denoBin == "" {
		denoBin = "deno"
	}
	kernelJSON := map[string]interface{}{
		"argv":         []string{denoBin, "jupyter", "--kernel", "--conn", "{connection_file}"},
		"display_name": spec.DisplayName,
		"language":     spec.Language,
		"name":         spec.KernelName,
	}
	data, err := json.MarshalIndent(kernelJSON, "", "  ")
	if err != nil {
		return fmt.Errorf("edge: marshal kernel.json: %w", err)
	}
	return os.WriteFile(filepath.Join(outDir, "kernel.json"), data, 0o644)
}

// EmitDenoJSON writes a deno.json that includes the Jupyter kernel entry.
func EmitDenoJSON(packageName, version, entrypoint, outDir string) error {
	denoJSON := map[string]interface{}{
		"name":    packageName,
		"version": version,
		"exports": map[string]string{".": "./" + entrypoint},
		"tasks": map[string]string{
			"jupyter": "deno jupyter --kernel",
		},
	}
	data, err := json.MarshalIndent(denoJSON, "", "  ")
	if err != nil {
		return fmt.Errorf("edge: marshal deno.json: %w", err)
	}
	return os.WriteFile(filepath.Join(outDir, "deno.json"), data, 0o644)
}
