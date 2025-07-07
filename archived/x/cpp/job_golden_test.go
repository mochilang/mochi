//go:build archived && slow

package cppcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_JOB_Golden(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"} {
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
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
		wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "cpp", q+".cpp.out"))
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.cpp.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
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
		wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "cpp", q+".out"))
		if err != nil {
			t.Fatalf("read golden output: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
