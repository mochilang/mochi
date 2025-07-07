//go:build archived && slow

package rktcode_test

import (
	"fmt"
	"testing"

	rktcode "mochi/archived/x/rkt"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_JOB(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return rktcode.New(env).Compile(prog)
		})
	}
}
