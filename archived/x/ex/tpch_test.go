//go:build archived && slow

package excode_test

import (
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_TPCH(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return excode.New(env).Compile(prog)
	})
}
