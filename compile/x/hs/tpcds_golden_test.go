//go:build slow

package hscode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	hscode "mochi/compile/x/hs"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestHSCompiler_TPCDSQueries(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
    root := testutil.FindRepoRoot(t)
    queries := []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"}
    for _, q := range queries {
            t.Run(q, func(t *testing.T) {
                    src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
                    prog, err := parser.Parse(src)
                    if err != nil {
                            t.Fatalf("parse error: %v", err)
                    }
                    env := types.NewEnv(nil)
                    if errs := types.Check(prog, env); len(errs) > 0 {
                            t.Fatalf("type error: %v", errs[0])
                    }
                    code, err := hscode.New(env).Compile(prog)
                    if err != nil {
                            t.Fatalf("compile error: %v", err)
                    }
                    codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "hs", q+".hs.out")
                    wantCode, err := os.ReadFile(codeWantPath)
                    if err != nil {
                            t.Fatalf("read golden: %v", err)
                    }
                    if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
                            t.Errorf("generated code mismatch for %s.hs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
                    }
                    dir := t.TempDir()
                    file := filepath.Join(dir, "main.hs")
                    if err := os.WriteFile(file, code, 0644); err != nil {
                            t.Fatalf("write error: %v", err)
                    }
                    t.Skip("Haskell runtime check disabled until heterogeneous maps are supported")
            })
    }
}
