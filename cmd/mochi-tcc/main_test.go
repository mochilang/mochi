package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestMochiTCCPrintHello(t *testing.T) {
	if _, err := exec.LookPath("tcc"); err != nil {
		t.Skip("tcc not installed")
	}
	tmp := t.TempDir()
	exe := filepath.Join(tmp, "mochi-tcc")
	root := findRepoRoot(t)
	build := exec.Command("go", "build", "-tags", "tcc libtcc", "-o", exe, filepath.Join(root, "cmd", "mochi-tcc"))
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("build mochi-tcc: %v\n%s", err, out)
	}

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	bin := filepath.Join(tmp, "print_hello")
	if out, err := exec.Command(exe, src, bin).CombinedOutput(); err != nil {
		t.Fatalf("compile error: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	wantData, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", "print_hello.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want := strings.TrimSpace(string(wantData))
	if got != want {
		t.Fatalf("output mismatch\nwant: %s\n got: %s", want, got)
	}
}

func findRepoRoot(t *testing.T) string {
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
