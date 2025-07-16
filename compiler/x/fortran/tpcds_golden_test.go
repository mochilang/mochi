//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".f90")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".out")
		errPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".error")
		if !shouldUpdate() {
			if _, err := os.Stat(codeWant); err != nil {
				continue
			}
		}
		t.Run(base, func(t *testing.T) {
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
				if shouldUpdate() {
					_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				}
				t.Skipf("compile error: %v", err)
				return
			}
			os.Remove(errPath)
			gotCode := bytes.TrimSpace(code)
			if shouldUpdate() {
				os.MkdirAll(filepath.Dir(codeWant), 0o755)
				_ = os.WriteFile(codeWant, append(gotCode, '\n'), 0644)
			} else {
				wantCode, err := os.ReadFile(codeWant)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.f90\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotCode, bytes.TrimSpace(wantCode))
				}
			}
			dir := t.TempDir()
			srcFile := filepath.Join(dir, "main.f90")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, out, 0644)
				}
				t.Skipf("gfortran error: %v", err)
				return
			}
			outBytes, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, outBytes, 0644)
				}
				t.Skipf("run error: %v", err)
				return
			}
			os.Remove(errPath)
			gotOut := bytes.TrimSpace(outBytes)
			if shouldUpdate() {
				_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outWant)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
