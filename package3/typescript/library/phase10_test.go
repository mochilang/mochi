package library_test

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/library"
)

// TestPhase10Library is the MEP-72 Phase 10 gate for the library package.
func TestPhase10Library(t *testing.T) {
	ts := `export function add(a: number, b: number): number { return a + b; }
export function greet(name: string): string { return "Hello, " + name; }
`
	t.Run("npm", func(t *testing.T) {
		outDir := t.TempDir()
		spec := library.NpmLibrarySpec{
			Name:        "@mochi/math",
			Version:     "1.0.0",
			Description: "Mochi math bridge",
			TypeScript:  ts,
		}
		if err := library.TargetNpmLibrary(spec, outDir); err != nil {
			t.Fatalf("TargetNpmLibrary: %v", err)
		}

		// package.json exists with correct exports map.
		pkgData, err := os.ReadFile(filepath.Join(outDir, "package.json"))
		if err != nil {
			t.Fatalf("package.json not found: %v", err)
		}
		var pkg map[string]interface{}
		if err := json.Unmarshal(pkgData, &pkg); err != nil {
			t.Fatalf("parse package.json: %v", err)
		}
		if pkg["name"] != "@mochi/math" {
			t.Errorf("name = %v", pkg["name"])
		}
		if _, ok := pkg["exports"]; !ok {
			t.Error("exports field missing")
		}

		// src/index.ts exists.
		srcData, err := os.ReadFile(filepath.Join(outDir, "src", "index.ts"))
		if err != nil {
			t.Fatalf("src/index.ts not found: %v", err)
		}
		if !strings.Contains(string(srcData), "export function add") {
			t.Error("src/index.ts missing add function")
		}

		// dist/index.d.ts exists.
		if _, err := os.Stat(filepath.Join(outDir, "dist", "index.d.ts")); err != nil {
			t.Error("dist/index.d.ts not found")
		}
	})

	t.Run("jsr", func(t *testing.T) {
		outDir := t.TempDir()
		spec := library.JsrLibrarySpec{
			Name:       "@mochi/math",
			Version:    "1.0.0",
			TypeScript: ts,
		}
		if err := library.TargetJsrLibrary(spec, outDir); err != nil {
			t.Fatalf("TargetJsrLibrary: %v", err)
		}
		denoData, err := os.ReadFile(filepath.Join(outDir, "deno.json"))
		if err != nil {
			t.Fatalf("deno.json not found: %v", err)
		}
		var deno map[string]interface{}
		if err := json.Unmarshal(denoData, &deno); err != nil {
			t.Fatalf("parse deno.json: %v", err)
		}
		if deno["name"] != "@mochi/math" {
			t.Errorf("name = %v", deno["name"])
		}
		exports, ok := deno["exports"].(map[string]interface{})
		if !ok {
			t.Fatal("exports not a map")
		}
		if exports["."] == "" {
			t.Error("exports['.'] empty")
		}
	})
}
