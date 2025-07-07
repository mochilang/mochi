//go:build archived && slow

package fscode_test

import (
	"testing"

	fscode "mochi/archived/x/fs"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_TPCH(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return fscode.New(env).Compile(prog)
			})
		})
	}
}
