package logic

import (
	"os"
	"path/filepath"
	"testing"
)

func TestGenerate(t *testing.T) {
	outDir := t.TempDir()
	err := GenerateFiles([]string{"evidence/slt_lang_update.test"}, outDir, true)
	if err != nil {
		t.Fatalf("generate failed: %v", err)
	}
	outPath := filepath.Join(outDir, "slt_lang_update", "case1.out")
	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read output: %v", err)
	}
	got := string(data)
	if got != "3\n" {
		t.Fatalf("unexpected output: %q", got)
	}
}
