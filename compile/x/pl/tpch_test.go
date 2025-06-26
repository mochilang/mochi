//go:build slow

package plcode_test

import (
	"testing"

	plcode "mochi/compile/x/pl"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_TPCH(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return plcode.New(env).Compile(prog)
	})
}
