//go:build archived && slow

package cppcode_test

import (
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_JOB(t *testing.T) {
	if _, err := cppcode.EnsureCPP(); err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cppcode.New(env).Compile(prog)
		})
	}
}
