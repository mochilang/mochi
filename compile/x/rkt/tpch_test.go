//go:build slow

package rktcode_test

import (
	"testing"

	rktcode "mochi/compile/x/rkt"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TPCH(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return rktcode.New(env).Compile(prog)
	})
}
