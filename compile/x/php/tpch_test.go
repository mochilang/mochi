//go:build slow

package phpcode_test

import (
	"testing"

	phpcode "mochi/compile/x/php"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_TPCH(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return phpcode.New(env).Compile(prog)
	})
}
