//go:build archived && slow

package ccode_test

import (
	"fmt"
	"testing"

	ccode "mochi/archived/x/c"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCCompiler_JOB(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		query := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, query, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return ccode.New(env).Compile(prog)
		})
	}
}
