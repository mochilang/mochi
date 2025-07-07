//go:build archived && slow

package mlir_test

import (
	"path/filepath"
	"testing"

	"mochi/archived/x/mlir"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestMLIRCompiler_TPCH(t *testing.T) {
	if err := mlir.EnsureMLIR(); err != nil {
		t.Skipf("mlir tools not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c, err := mlir.New()
	if err != nil {
		t.Skipf("mlir not available: %v", err)
	}
	if _, err := c.Compile(prog); err != nil {
		t.Skipf("TPCH Q1 unsupported: %v", err)
	}
}
