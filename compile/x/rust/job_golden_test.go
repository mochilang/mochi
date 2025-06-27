//go:build slow

package rscode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	rscode "mochi/compile/x/rust"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_JOB(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
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
		code, err := rscode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", q+".rs.out")
		wantCode, err := os.ReadFile(wantCodePath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.rs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
	}
}
