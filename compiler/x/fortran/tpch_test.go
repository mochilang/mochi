//go:build slow

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

func TestFortranCompiler_TPCH(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	q := "q1"
	t.Run(q, func(t *testing.T) {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
		wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "fortran", q+".f90.out")
		wantCode, err := os.ReadFile(wantCodePath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		dir := t.TempDir()
		srcFile := filepath.Join(dir, "main.f90")
		if err := os.WriteFile(srcFile, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command(gfortran, srcFile, "-o", exe).CombinedOutput(); err != nil {
			t.Fatalf("gfortran error: %v\n%s", err, out)
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			t.Fatalf("run error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", q+".out")
		wantOut, err := os.ReadFile(wantOutPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	})
}
