//go:build slow

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compile/x/c"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestCCompiler_SLT_Golden(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	groups := []string{"select1"}
	for _, g := range groups {
		for i := 1; i <= 10; i++ {
			caseName := fmt.Sprintf("case%d", i)
			src := filepath.Join(root, "tests", "dataset", "slt", "out", g, caseName+".mochi")
			if _, err := os.Stat(src); err != nil {
				continue
			}
			t.Run(caseName, func(t *testing.T) {
				if _, err := os.Stat(strings.TrimSuffix(src, ".mochi") + ".error"); err == nil {
					t.Skip("error output")
				}
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
				}
				tmp := t.TempDir()
				srcFile := filepath.Join(tmp, "prog.c")
				if err := os.WriteFile(srcFile, code, 0644); err != nil {
					t.Fatalf("write error: %v", err)
				}
				bin := filepath.Join(tmp, "prog")
				if out, err := exec.Command(cc, srcFile, "-o", bin).CombinedOutput(); err != nil {
					t.Skipf("cc error: %v\n%s", err, out)
				}
				out, err := exec.Command(bin).CombinedOutput()
				if err != nil {
					t.Skipf("run error: %v\n%s", err, out)
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
							t.Errorf("vm mismatch for %s/%s\n\n--- VM ---\n%s\n\n--- C ---\n%s\n", g, caseName, vmOut, gotOut)
						}
					}
				}
				wantOutPath := filepath.Join(root, "tests", "dataset", "slt", "out", g, caseName+".out")
				wantOut, err := os.ReadFile(wantOutPath)
				if err != nil {
					t.Fatalf("read golden output: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s/%s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", g, caseName, gotOut, bytes.TrimSpace(wantOut))
				}
			})
		}
	}
}
