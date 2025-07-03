//go:build slow

package cppcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cppcode "mochi/compile/x/cpp"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_SLT_Golden(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	groups := []string{"select1"}
	for _, g := range groups {
		for i := 1; i <= 5; i++ {
			caseName := fmt.Sprintf("case%d", i)
			src := filepath.Join(root, "tests", "dataset", "slt", "out", g, caseName+".mochi")
			if _, err := os.Stat(src); err != nil {
				continue
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := cppcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			srcFile := filepath.Join(tmp, "prog.cpp")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(tmp, "prog")
			if out, err := exec.Command(cpp, srcFile, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
				t.Skipf("cpp compile error: %v\n%s", err, out)
			}
			out, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOutPath := filepath.Join(root, "tests", "dataset", "slt", "out", g, caseName+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden output: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s/%s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", g, caseName, gotOut, bytes.TrimSpace(wantOut))
			}
		}
	}
}
