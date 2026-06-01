package emit_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/apisurface"
	"mochi/package3/typescript/emit"
	brerrors "mochi/package3/typescript/errors"
	"mochi/package3/typescript/typemap"
)

// TestPhase6Emit is the MEP-72 Phase 6 gate for the emit package.
func TestPhase6Emit(t *testing.T) {
	surface := &apisurface.ApiSurface{
		PackageName: "lodash",
		Version:     "4.17.21",
		Exports:     map[string][]apisurface.Export{},
	}

	mapped := []typemap.MappedExport{
		{
			Export: apisurface.Export{
				Kind: "function",
				Name: "chunk",
				Params: []apisurface.Param{
					{Name: "array", TypeStr: "any[]"},
					{Name: "size", TypeStr: "number"},
				},
				ReturnType: "any[][]",
			},
			MochiName: "ts_lodash_chunk",
			ParamTypes: []typemap.MapResult{
				{Type: "list", Container: "list"},
				{Type: typemap.TypeInt},
			},
			ReturnType: typemap.MapResult{Type: "list", Container: "list"},
		},
	}

	outDir := t.TempDir()
	externsPath := filepath.Join(outDir, "externs.mochi")
	if err := emit.EmitExterns(surface, mapped, externsPath); err != nil {
		t.Fatalf("EmitExterns: %v", err)
	}

	data, err := os.ReadFile(externsPath)
	if err != nil {
		t.Fatalf("read externs: %v", err)
	}
	content := string(data)
	if !strings.Contains(content, "extern ts fun ts_lodash_chunk") {
		t.Errorf("externs missing ts_lodash_chunk:\n%s", content)
	}
	if !strings.Contains(content, "lodash") {
		t.Errorf("externs missing package name comment:\n%s", content)
	}

	// Test EmitSkipped.
	skipped := []brerrors.SkipReport{
		{ItemPath: "someGeneric", Reason: brerrors.SkipGeneric},
		{ItemPath: "deprecated", Reason: brerrors.SkipDeprecated},
	}
	skippedPath := filepath.Join(outDir, "SKIPPED.txt")
	if err := emit.EmitSkipped(skipped, skippedPath); err != nil {
		t.Fatalf("EmitSkipped: %v", err)
	}
	skData, err := os.ReadFile(skippedPath)
	if err != nil {
		t.Fatalf("read skipped: %v", err)
	}
	skContent := string(skData)
	if !strings.Contains(skContent, "SkipGeneric") {
		t.Errorf("SKIPPED.txt missing SkipGeneric:\n%s", skContent)
	}
	if !strings.Contains(skContent, "deprecated") {
		t.Errorf("SKIPPED.txt missing deprecated:\n%s", skContent)
	}
}
