package mochi_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	mochi "mochi/aster/x/mochi"
)

func repoRootDecorate(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestDecorate(t *testing.T) {
	root := repoRootDecorate(t)
	inPath := filepath.Join(root, "tests", "aster", "x", "mochi", "decorator.in.mochi")
	outPath := filepath.Join(root, "tests", "aster", "x", "mochi", "decorator.out.mochi")
	data, err := os.ReadFile(inPath)
	if err != nil {
		t.Fatalf("read in: %v", err)
	}
	prog, err := mochi.Inspect(string(data))
	if err != nil {
		t.Fatalf("inspect: %v", err)
	}
	dec, err := mochi.Decorate(prog)
	if err != nil {
		t.Fatalf("decorate: %v", err)
	}
	printed, err := mochi.Print(dec)
	if err != nil {
		t.Fatalf("print: %v", err)
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read out: %v", err)
	}
	if strings.TrimSpace(printed) != strings.TrimSpace(string(want)) {
		t.Fatalf("mismatch\n--- got ---\n%s\n--- want ---\n%s", printed, want)
	}
}
