//go:build slow

package ktcode_test

import (
	"testing"

	ktcode "mochi/compile/x/kt"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestKTCompiler_TPCH(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ktcode.New(env).Compile(prog)
	})
}
