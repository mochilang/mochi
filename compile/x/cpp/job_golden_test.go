//go:build slow

package cppcode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	cppcode "mochi/compile/x/cpp"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_JOB_Golden(t *testing.T) {
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for _, q := range []string{"q1", "q2"} {
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
		_ = cpp
	}
}
