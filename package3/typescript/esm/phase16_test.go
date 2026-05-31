package esm_test

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/esm"
)

// TestPhase16ESM is the MEP-72 Phase 16 gate for the esm package.
func TestPhase16ESM(t *testing.T) {
	t.Run("exports_map_esm", func(t *testing.T) {
		dir := t.TempDir()
		pkg := map[string]interface{}{
			"name":    "my-pkg",
			"version": "1.0.0",
			"type":    "module",
			"exports": map[string]interface{}{
				".": map[string]string{
					"import":  "./dist/index.mjs",
					"require": "./dist/index.js",
				},
			},
		}
		writePkgJSON(t, dir, pkg)

		ep, err := esm.ResolveEntryPoint(dir)
		if err != nil {
			t.Fatalf("ResolveEntryPoint: %v", err)
		}
		if ep.Format != esm.FormatESM {
			t.Errorf("format = %v, want ESM", ep.Format)
		}
		if !strings.HasSuffix(ep.Path, "index.mjs") {
			t.Errorf("path = %q, want ...index.mjs", ep.Path)
		}
	})

	t.Run("main_cjs_fallback", func(t *testing.T) {
		dir := t.TempDir()
		pkg := map[string]interface{}{
			"name":    "old-pkg",
			"version": "1.0.0",
			"main":    "./index.js",
		}
		writePkgJSON(t, dir, pkg)

		ep, err := esm.ResolveEntryPoint(dir)
		if err != nil {
			t.Fatalf("ResolveEntryPoint: %v", err)
		}
		if ep.Format != esm.FormatCJS {
			t.Errorf("format = %v, want CJS", ep.Format)
		}
	})

	t.Run("module_field_esm", func(t *testing.T) {
		dir := t.TempDir()
		pkg := map[string]interface{}{
			"name":   "dual-pkg",
			"main":   "./index.cjs",
			"module": "./index.mjs",
		}
		writePkgJSON(t, dir, pkg)

		ep, err := esm.ResolveEntryPoint(dir)
		if err != nil {
			t.Fatalf("ResolveEntryPoint: %v", err)
		}
		if ep.Format != esm.FormatESM {
			t.Errorf("format = %v, want ESM", ep.Format)
		}
		if !strings.HasSuffix(ep.Path, "index.mjs") {
			t.Errorf("path = %q, want ...index.mjs", ep.Path)
		}
	})

	t.Run("wrap_cjs_for_esm", func(t *testing.T) {
		outDir := t.TempDir()
		outPath := filepath.Join(outDir, "wrapper.mjs")
		if err := esm.WrapCJSForESM("/abs/path/index.js", outPath); err != nil {
			t.Fatalf("WrapCJSForESM: %v", err)
		}
		data, err := os.ReadFile(outPath)
		if err != nil {
			t.Fatalf("read wrapper: %v", err)
		}
		content := string(data)
		if !strings.Contains(content, "createRequire") {
			t.Error("wrapper missing createRequire")
		}
	})
}

func writePkgJSON(t *testing.T, dir string, pkg interface{}) {
	t.Helper()
	data, err := json.MarshalIndent(pkg, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "package.json"), data, 0o644); err != nil {
		t.Fatal(err)
	}
}
