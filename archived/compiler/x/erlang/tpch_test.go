//go:build archived && slow

package erlcode_test

import (
	"testing"

	erlcode "mochi/archived/x/erlang"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestErlangCompiler_TPCH(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return erlcode.New(env).Compile(prog)
	})
}
