//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ftncode "mochi/compiler/x/fortran"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFortranCompiler_TPCDSQueries(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".f90.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		if _, err := os.Stat(outWant); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			os.Setenv("MOCHI_FORTRAN_NODATASET", "1")
			code, err := ftncode.New(env).Compile(prog)
			os.Unsetenv("MOCHI_FORTRAN_NODATASET")
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(bytes.TrimSpace(code))
			want := stripHeader(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.f90.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.f90")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(gfortran, file, "-static", "-o", exe).CombinedOutput(); err != nil {
				t.Fatalf("gfortran error: %v\n%s", err, out)
			}
			cmd := exec.Command(exe)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
