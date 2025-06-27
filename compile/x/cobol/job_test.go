//go:build slow

package cobolcode_test

import (
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_JOB(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cobolcode.New(env).Compile(prog)
		})
	}
}
