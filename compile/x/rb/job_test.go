//go:build slow

package rbcode_test

import (
	"fmt"
	"testing"

	rbcode "mochi/compile/x/rb"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_JOB(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return rbcode.New(env).Compile(prog)
		})
	}
}
