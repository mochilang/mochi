//go:build slow

package fortran_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	ftn "mochi/transpiler/x/fortran"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func TestTranspilePrintHello(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	f90Path := filepath.Join(outDir, "print_hello.f90")
	if err := os.WriteFile(f90Path, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	exe := filepath.Join(outDir, "print_hello")
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("compile error: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}
