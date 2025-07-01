//go:build slow

package pascode_test

import (
	"testing"

	pascode "mochi/compile/x/pas"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_TPCDS(t *testing.T) {
	if _, err := pascode.EnsureFPC(); err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	testutil.CompileTPCDS(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return pascode.New(env).Compile(prog)
	})
}
