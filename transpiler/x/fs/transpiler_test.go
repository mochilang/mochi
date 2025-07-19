package fstrans_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

func TestFSTranspiler_PrintHello(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	wantPath := filepath.Join(root, "tests", "transpiler", "x", "fs", "print_hello.out")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatal(errs[0])
	}
	ast, err := fstrans.Transpile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	code := fstrans.Emit(ast)

	dir := t.TempDir()
	fsFile := filepath.Join(dir, "main.fs")
	if err := os.WriteFile(fsFile, code, 0644); err != nil {
		t.Fatal(err)
	}
	exe := filepath.Join(dir, "main.exe")
	cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exe, fsFile)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("fsharpc error: %v\n%s", err, out)
	}
	run := exec.Command("mono", exe)
	out, err := run.CombinedOutput()
	if err != nil {
		t.Fatalf("mono error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
	}

	// update golden source file for reference
	goldenSrc := filepath.Join(root, "tests", "transpiler", "x", "fs", "print_hello.fs")
	_ = os.WriteFile(goldenSrc, code, 0644)
}
