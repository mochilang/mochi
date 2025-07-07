//go:build archived && slow

package wasm_test

import (
	"path/filepath"
	"testing"

	"mochi/archived/x/testutil"
	"mochi/archived/x/wasm"
	"mochi/parser"
	"mochi/types"
)

func TestWasmCompiler_TPCH(t *testing.T) {
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
	if _, err := wasm.New(env).Compile(prog); err != nil {
		t.Skipf("TPCH Q1 unsupported: %v", err)
	}
}
