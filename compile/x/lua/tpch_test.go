//go:build slow

package luacode_test

import (
	"testing"

	luacode "mochi/compile/x/lua"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_TPCH(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return luacode.New(env).Compile(prog)
	})
}
