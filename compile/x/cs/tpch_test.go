//go:build slow

package cscode_test

import (
	"testing"

	cscode "mochi/compile/x/cs"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_TPCH(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cscode.New(env).Compile(prog)
	})
}
