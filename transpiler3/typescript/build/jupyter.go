// Phase 17.1: Jupyter kernelspec emit for the Deno-backed Mochi
// kernel.
//
// Deno's official Jupyter kernel (`deno jupyter --install`, GA
// April 2024) executes TypeScript notebooks. The Mochi kernel
// piggybacks on it: each Mochi cell is transpiled to TS by a
// sidecar process and the resulting TS is forwarded to Deno's
// kernel.
//
// emitKernelspec writes `kernel.json` to outDir. Physical install
// into `~/.local/share/jupyter/kernels/mochi-deno-<pkg>/` is a
// one-time user step, mirrored by `jupyter kernelspec install`,
// and is intentionally NOT performed by the build driver. The
// driver's job is to produce a manifest the user (or CI) can
// install with a single shell command.
package build

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// emitKernelspec writes <outDir>/kernel.json describing the Mochi
// Deno kernel. The argv invokes `deno jupyter` with the unstable
// flag (Deno's Jupyter integration is still unstable in 2.x),
// passes the connection_file placeholder, and grants read + net +
// env permissions so notebooks can do useful work without
// per-cell --allow-* prompts.
//
// The display_name is "Mochi (Deno)" so the kernel is
// distinguishable in the JupyterLab kernel chooser. The language
// is "mochi" so notebook front-ends can colourise cells with a
// custom codemirror mode.
func emitKernelspec(outDir, pkgName string) (string, error) {
	if pkgName == "" {
		return "", fmt.Errorf("ts emit kernelspec: empty package name")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("ts emit kernelspec: mkdir %s: %w", outDir, err)
	}
	spec := map[string]any{
		"argv": []string{
			"deno",
			"jupyter",
			"--unstable",
			"--kernel",
			"{connection_file}",
			"--allow-read",
			"--allow-net",
			"--allow-env",
		},
		"display_name": "Mochi (Deno)",
		"language":     "mochi",
		"metadata": map[string]any{
			"mochi_package": pkgName,
		},
	}
	body, err := json.MarshalIndent(spec, "", "  ")
	if err != nil {
		return "", fmt.Errorf("ts emit kernelspec: marshal: %w", err)
	}
	body = append(body, '\n')
	abs := filepath.Join(outDir, "kernel.json")
	if err := os.WriteFile(abs, body, 0o644); err != nil {
		return "", fmt.Errorf("ts emit kernelspec: write %s: %w", abs, err)
	}
	return abs, nil
}
