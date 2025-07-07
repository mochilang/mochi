//go:build archived && slow

package cppcode_test

import (
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_TPCH(t *testing.T) {
	if _, err := cppcode.EnsureCPP(); err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cppcode.New(env).Compile(prog)
	})
}
