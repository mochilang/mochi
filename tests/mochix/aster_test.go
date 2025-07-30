//go:build slow

package mochix_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

func TestInspectGoPrintHello(t *testing.T) {
	root := repoRoot(t)
	src := filepath.Join(root, "tests/transpiler/x/go/print_hello.go")
	got, err := runMochix(t, "inspect", "go", src)
	if err != nil {
		t.Fatalf("inspect error: %v", err)
	}
	wantPath := filepath.Join(root, "tests/aster/x/go/print_hello.go.json")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Fatalf("golden mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}
