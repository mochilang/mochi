//go:build slow

package excode_test

import (
	"fmt"
	"testing"

	excode "mochi/compile/x/ex"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_JOB(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return excode.New(env).Compile(prog)
		})
	}
}
