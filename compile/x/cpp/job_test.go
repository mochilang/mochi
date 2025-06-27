//go:build slow

package cppcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cppcode "mochi/compile/x/cpp"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func compileAndRun(t *testing.T, query string) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", query+".mochi")
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
	wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cpp", query+".cpp.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
		t.Errorf("generated code mismatch for %s.cpp.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query, got, bytes.TrimSpace(want))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.cpp")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "prog")
	if out, err := exec.Command(cpp, file, "-std=c++17", "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("cpp error: %v\n%s", err, out)
	}
	cmd := exec.Command(exe)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cpp", query+".out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query, gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestCPPCompiler_JOBQ1(t *testing.T) { compileAndRun(t, "q1") }
func TestCPPCompiler_JOBQ2(t *testing.T) { compileAndRun(t, "q2") }
