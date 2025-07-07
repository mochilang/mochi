//go:build archived && slow

package dartcode_test

import (
	"testing"

	dartcode "mochi/archived/x/dart"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_TPCH(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return dartcode.New(env).Compile(prog)
	})
}
