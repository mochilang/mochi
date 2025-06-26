//go:build slow

package stcode_test

import (
	"testing"

	stcode "mochi/compile/x/st"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSTCompiler_TPCH(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return stcode.New(env).Compile(prog)
	})
}
