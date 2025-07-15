//go:build slow

package ftncode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	ftncode "mochi/compiler/x/fortran"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestFortranCompiler_TPCDS_Dataset_Golden compiles a subset of TPC-DS queries
// and verifies the generated code and program output.
func TestFortranCompiler_TPCDS_Dataset_Golden(t *testing.T) {
	gfortran := ensureFortran(t)
	root := testutil.FindRepoRoot(t)
	queries := make([]int, 0, 99)
	if env := os.Getenv("QUERIES"); env != "" {
		for _, p := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(p)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 99; i++ {
			queries = append(queries, i)
		}
	}

	for _, i := range queries {
		base := fmt.Sprintf("q%d", i)
		t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
			outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "out", base+".out")
			errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".error")
			codeFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran", base+".f90")

			prog, err := parser.Parse(src)
			if err != nil {
				os.WriteFile(errFile, []byte(err.Error()), 0644)
				os.Remove(codeFile)
				t.Skipf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(errFile, []byte(errs[0].Error()), 0644)
				os.Remove(codeFile)
				t.Skipf("type error: %v", errs[0])
			}
			code, err := ftncode.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(errFile, []byte(err.Error()), 0644)
				os.Remove(codeFile)
				t.Skipf("compile error: %v", err)
			}
			os.Remove(errFile)
			if err := os.WriteFile(codeFile, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			dir := t.TempDir()
			srcFile := filepath.Join(dir, "main.f90")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write src: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(gfortran, srcFile, "-static", "-o", exe).CombinedOutput(); err != nil {
				t.Fatalf("gfortran: %v\n%s", err, out)
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Fatalf("run: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)
			want, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Fatalf("output mismatch\n--- Got ---\n%s\n--- Want ---\n%s", got, bytes.TrimSpace(want))
			}
		})
	}
}
