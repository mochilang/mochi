//go:build slow

package cscode_test

import (
	parser "mochi/parser"
	"mochi/types"
	"testing"

	cscode "mochi/compile/x/cs"
	"mochi/compile/x/testutil"
)

func TestCSCompiler_JOB(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	testutil.CompileJOB(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cscode.New(env).Compile(prog)
	})
	testutil.CompileJOB(t, "q2", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cscode.New(env).Compile(prog)
	})
}
