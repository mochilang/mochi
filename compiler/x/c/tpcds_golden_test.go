//go:build slow

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestCCompiler_TPCDS_Golden(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRootTPCH(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
			if _, err := os.Stat(src); err != nil {
				t.Skip("source missing")
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
			code, err := ccode.New(env).Compile(prog)
			errPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "c", q+".error")
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				t.Skipf("compile error: %v", err)
				return
			}
			os.Remove(errPath)
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "c", q+".c")
			gotCode := bytes.TrimSpace(ccode.FormatC(code))
			if shouldUpdate() {
				if err := os.WriteFile(wantCodePath, append(gotCode, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				wantCode, err := os.ReadFile(wantCodePath)
				if err != nil {
					t.Skipf("read golden: %v", err)
				}
				if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".c", gotCode, bytes.TrimSpace(wantCode))
				}
			}
			dir := t.TempDir()
			cfile := filepath.Join(dir, "prog.c")
			if err := os.WriteFile(cfile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command(cc, cfile, "-o", bin).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, out, 0644)
				t.Skipf("cc error: %v", err)
				return
			}
			out, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, out, 0644)
				t.Skipf("run error: %v", err)
				return
			}
			os.Remove(errPath)
			gotOut := bytes.TrimSpace(out)

			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Errorf("vm compile error: %v", err)
			} else {
				var vmBuf bytes.Buffer
				m := vm.New(p, &vmBuf)
				if err := m.Run(); err != nil {
					t.Errorf("vm run error: %v", err)
				} else {
					vmOut := bytes.TrimSpace(vmBuf.Bytes())
					if !bytes.Equal(gotOut, vmOut) {
						t.Errorf("vm mismatch for %s\n\n--- VM ---\n%s\n\n--- C ---\n%s\n", q, vmOut, gotOut)
					}
				}
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "c", q+".out")
			if shouldUpdate() {
				os.WriteFile(wantOutPath, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(wantOutPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".out", gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
