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
	queries := []string{
		"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9",
		"q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19",
		"q20", "q21", "q22", "q23", "q25", "q26", "q27", "q28", "q29",
		"q30", "q31", "q32", "q33", "q34", "q35", "q36", "q37", "q38",
		"q40", "q41", "q42", "q43", "q44", "q45", "q46", "q47", "q48", "q49",
	}
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
