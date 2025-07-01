//go:build slow

package cppcode_test

import (
	"testing"

	cppcode "mochi/compile/x/cpp"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_TPCDS(t *testing.T) {
	if _, err := cppcode.EnsureCPP(); err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	testutil.CompileTPCDS(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cppcode.New(env).Compile(prog)
	})
}
