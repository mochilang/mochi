//go:build slow

package rbcode_test

import (
	"testing"

	rbcode "mochi/compile/x/rb"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_TPCH(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return rbcode.New(env).Compile(prog)
	})
}
