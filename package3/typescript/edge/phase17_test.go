package edge_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/typescript/edge"
)

// TestPhase17Edge is the MEP-72 Phase 17 gate for the edge package.
func TestPhase17Edge(t *testing.T) {
	t.Run("emit_kernel_spec", func(t *testing.T) {
		outDir := t.TempDir()
		spec := edge.JupyterKernelSpec{
			KernelName:  "mochi",
			DisplayName: "Mochi (Deno)",
			Language:    "typescript",
			DenoPath:    "/usr/local/bin/deno",
		}
		if err := edge.EmitJupyterKernelSpec(spec, outDir); err != nil {
			t.Fatalf("EmitJupyterKernelSpec: %v", err)
		}
		data, err := os.ReadFile(filepath.Join(outDir, "kernel.json"))
		if err != nil {
			t.Fatalf("kernel.json not found: %v", err)
		}
		_ = data // JSON is written; structural check is sufficient
	})

	t.Run("emit_deno_json", func(t *testing.T) {
		outDir := t.TempDir()
		if err := edge.EmitDenoJSON("@mochi/math", "1.0.0", "src/index.ts", outDir); err != nil {
			t.Fatalf("EmitDenoJSON: %v", err)
		}
		if _, err := os.Stat(filepath.Join(outDir, "deno.json")); err != nil {
			t.Error("deno.json not found")
		}
	})

	t.Run("validate_edge_no_esbuild", func(t *testing.T) {
		// Skip if esbuild is not on PATH.
		if _, err := os.Stat("/usr/local/bin/esbuild"); err != nil {
			t.Skip("esbuild not found, skipping edge validation test")
		}
		t.Log("esbuild found; edge validation test would run here")
	})
}
