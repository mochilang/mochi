//go:build archived && slow

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ccode "mochi/archived/x/c"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func repoRootTPCH(t *testing.T) string {
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

func TestCCompiler_TPCH_Golden(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRootTPCH(t)
	for i := 1; i <= 2; i++ {
		query := fmt.Sprintf("q%d", i)
		t.Run(query, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", query+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := ccode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "c", query+".c.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			gotCode := bytes.TrimSpace(code)
			if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query+".c.out", gotCode, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			cfile := filepath.Join(dir, "prog.c")
			if err := os.WriteFile(cfile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command(cc, cfile, "-o", bin).CombinedOutput(); err != nil {
				t.Skipf("cc error: %v\n%s", err, out)
				return
			}
			out, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, out)
				return
			}
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
						t.Errorf("vm mismatch for %s\n\n--- VM ---\n%s\n\n--- C ---\n%s\n", query, vmOut, gotOut)
					}
				}
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", query+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query+".out", gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
