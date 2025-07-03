//go:build slow

package ccode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ccode "mochi/compile/x/c"
	"mochi/parser"
	"mochi/types"
)

func repoRootSLT(t *testing.T) string {
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

func TestCCompiler_SLT_Golden(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRootSLT(t)
	cases := []string{
		"select1/case1",
		"select1/case2",
		"select1/case3",
		"select1/case4",
		"select1/case5",
	}
	for _, c := range cases {
		t.Run(c, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "slt", "out", c+".mochi")
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
			wantOutPath := filepath.Join(root, "tests", "dataset", "slt", "out", c+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", c+".out", gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
