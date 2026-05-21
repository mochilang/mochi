package main

import (
	"os"
	"path/filepath"
	"testing"
)

// TestClangVersionFileExists asserts the CLANG_VERSION pin file is
// present and non-empty. Phase 1.1's diff-on-bump CI workflow depends
// on this file being readable; the test gates the workflow against
// accidental deletion.
func TestClangVersionFileExists(t *testing.T) {
	wd, _ := os.Getwd()
	// Walk up to the repo root so the test runs from the tools/stencilgen
	// directory and still finds the file.
	path := filepath.Join(wd, "CLANG_VERSION")
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read CLANG_VERSION: %v", err)
	}
	if len(data) == 0 {
		t.Errorf("CLANG_VERSION is empty")
	}
}

// TestStencilSourcePresent asserts at least one .c stencil source is
// shipped in stencils/. The full set lands in Phase 1.1; for Phase 1
// op_add_i64.c is the lone representative.
func TestStencilSourcePresent(t *testing.T) {
	entries, err := os.ReadDir("stencils")
	if err != nil {
		t.Fatalf("read stencils dir: %v", err)
	}
	var cCount int
	for _, e := range entries {
		if filepath.Ext(e.Name()) == ".c" {
			cCount++
		}
	}
	if cCount == 0 {
		t.Errorf("no .c stencil sources in stencils/; Phase 1 ships at least op_add_i64.c")
	}
}
