//go:build archived && slow

package pascode_test

import (
	"testing"

	pascode "mochi/archived/x/pas"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_TPCH(t *testing.T) {
	if _, err := pascode.EnsureFPC(); err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return pascode.New(env).Compile(prog)
	})
}
