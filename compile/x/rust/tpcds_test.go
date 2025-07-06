//go:build slow

package rscode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	rscode "mochi/compile/x/rust"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_TPCDS(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 19; i++ {
		q := fmt.Sprintf("q%d", i)
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
			code, err := rscode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rust", q+".rs.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.rs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}

			// Execute generated code and verify output
			gotRun := compileAndRunRust(t, src)
			wantRunPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rust", q+".out")
			wantRunBytes, err := os.ReadFile(wantRunPath)
			if err != nil {
				t.Fatalf("read output: %v", err)
			}
			wantRun := strings.TrimSpace(string(wantRunBytes))
			if gotRun != wantRun {
				t.Errorf("unexpected runtime output for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotRun, wantRun)
			}

			vmOut, err := runVM(src)
			if err != nil {
				t.Fatalf("vm error: %v", err)
			}
			if gotRun != strings.TrimSpace(string(vmOut)) {
				t.Errorf("vm mismatch for %s\n\n--- Rust ---\n%s\n\n--- VM ---\n%s\n", q, gotRun, vmOut)
			}
		})
	}
}
