//go:build archived && slow

package hscode_test

import (
	"testing"

	hscode "mochi/archived/x/hs"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestHSCompiler_TPCH(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return hscode.New(env).Compile(prog)
	})
}
