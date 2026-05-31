package docs_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/swift/docs"
)

// TestPhase18DocC is the MEP-69 Phase 18 gate for DocC documentation emit.
func TestPhase18DocC(t *testing.T) {
	spec := docs.CatalogSpec{
		ModuleName: "MochiMath",
		Version:    "1.0.0",
		Comments: []docs.DocComment{
			{
				Name:    "add",
				Kind:    "function",
				Summary: "Adds two integers.",
				Params: []docs.ParamDoc{
					{Name: "a", Description: "The first operand."},
					{Name: "b", Description: "The second operand."},
				},
				Returns: "The sum of a and b.",
			},
			{
				Name:    "greet",
				Kind:    "function",
				Summary: "Returns a greeting string.",
				Params:  []docs.ParamDoc{{Name: "name", Description: "The name to greet."}},
				Returns: "A greeting message.",
			},
		},
	}

	outDir := t.TempDir()
	if err := docs.EmitDocC(spec, outDir); err != nil {
		t.Fatalf("EmitDocC: %v", err)
	}

	doccDir := filepath.Join(outDir, "MochiMath.docc")

	// Module doc exists.
	moduleDoc, err := os.ReadFile(filepath.Join(doccDir, "MochiMath.md"))
	if err != nil {
		t.Fatalf("module doc not found: %v", err)
	}
	md := string(moduleDoc)
	if !strings.Contains(md, "``MochiMath``") {
		t.Error("module doc missing ``MochiMath``")
	}
	if !strings.Contains(md, "1.0.0") {
		t.Error("module doc missing version 1.0.0")
	}

	// Symbol docs exist.
	addDoc, err := os.ReadFile(filepath.Join(doccDir, "Symbols", "add.md"))
	if err != nil {
		t.Fatalf("add symbol doc not found: %v", err)
	}
	addMd := string(addDoc)
	if !strings.Contains(addMd, "``MochiMath/add``") {
		t.Error("add doc missing ``MochiMath/add``")
	}
	if !strings.Contains(addMd, "Adds two integers") {
		t.Error("add doc missing summary")
	}
	if !strings.Contains(addMd, "The first operand") {
		t.Error("add doc missing param docs")
	}
}
