package ftncode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ftncode "mochi/compiler/x/fortran"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestFortranCompiler_TPCH_Golden compiles the TPCH q1 example
// under tests/compiler/fortran and verifies the generated code
// and program output against the golden files.
func TestFortranCompiler_TPCH_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	base := "q1"
	src := filepath.Join(root, "tests", "compiler", "fortran", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ftncode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCodePath := filepath.Join(root, "tests", "compiler", "fortran", base+".f90.out")
	wantCode, err := os.ReadFile(wantCodePath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	srcFile := filepath.Join(dir, "main.f90")
	if err := os.WriteFile(srcFile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("gfortran error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	wantOutPath := filepath.Join(root, "tests", "compiler", "fortran", base+".out")
	wantOut, err := os.ReadFile(wantOutPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
	}
}
