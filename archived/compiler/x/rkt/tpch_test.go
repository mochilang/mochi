//go:build archived && slow

package rktcode_test

import (
	"testing"

	rktcode "mochi/archived/x/rkt"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TPCH(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return rktcode.New(env).Compile(prog)
		})
	}
}
