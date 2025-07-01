package excode_test

import (
	"testing"

	excode "mochi/compile/x/ex"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_TPCDS(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	testutil.CompileTPCDS(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return excode.New(env).Compile(prog)
	})
}
