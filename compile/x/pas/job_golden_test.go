//go:build slow

package pascode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	pascode "mochi/compile/x/pas"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_JOB_Golden(t *testing.T) {
	if _, err := pascode.EnsureFPC(); err != nil {
		t.Skipf("fpc not installed: %v", err)
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
		code, err := pascode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "pas", q+".pas.out")
		wantCode, err := os.ReadFile(wantPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		got := bytes.TrimSpace(code)
		if !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.pas.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		// The generated Pascal code does not yet compile cleanly
		// so runtime execution is skipped for now.
	}
}
