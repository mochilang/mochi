package monomorphise_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/monomorphise"
)

// TestPhase15Monomorphise is the MEP-72 Phase 15 gate for the monomorphise package.
func TestPhase15Monomorphise(t *testing.T) {
	sites := []monomorphise.CallSite{
		{FuncName: "wrap", Module: "my-pkg", TypeArgs: []string{"string"}},
		{FuncName: "wrap", Module: "my-pkg", TypeArgs: []string{"number"}},
		{FuncName: "convert", Module: "my-pkg", TypeArgs: []string{"boolean"}},
		{FuncName: "exotic", Module: "my-pkg", TypeArgs: []string{"MyCustomType"}},
	}

	funcs, err := monomorphise.MonomorphiseAll(sites)
	if err != nil {
		t.Fatalf("MonomorphiseAll: %v", err)
	}
	if len(funcs) != 4 {
		t.Fatalf("len = %d, want 4", len(funcs))
	}

	// string instantiation.
	if !strings.Contains(funcs[0].MangledName, "String") {
		t.Errorf("string mangled name = %q, want ...String", funcs[0].MangledName)
	}
	if funcs[0].IsSkipped {
		t.Error("string should not be skipped")
	}

	// number instantiation.
	if !strings.Contains(funcs[1].MangledName, "Number") {
		t.Errorf("number mangled name = %q", funcs[1].MangledName)
	}

	// Unsupported type → skipped.
	exotic := funcs[3]
	if !exotic.IsSkipped {
		t.Errorf("MyCustomType should be skipped")
	}

	// EmitMonomorphicShims.
	outDir := t.TempDir()
	outPath := filepath.Join(outDir, "mono.mjs")
	if err := monomorphise.EmitMonomorphicShims(funcs, "my-pkg", outPath); err != nil {
		t.Fatalf("EmitMonomorphicShims: %v", err)
	}
	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read shims: %v", err)
	}
	content := string(data)
	if !strings.Contains(content, "export function") {
		t.Error("shims missing export function")
	}
	if !strings.Contains(content, "SKIP") {
		t.Error("shims missing SKIP comment for exotic type")
	}
}
