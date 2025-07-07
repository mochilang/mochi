//go:build archived && slow

package rbcode_test

import (
	"testing"

	rbcode "mochi/archived/x/rb"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_TPCH(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return rbcode.New(env).Compile(prog)
			})
		})
	}
}
