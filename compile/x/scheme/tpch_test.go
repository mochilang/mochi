//go:build slow

package schemecode_test

import (
	"testing"

	schemecode "mochi/compile/x/scheme"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSchemeCompiler_TPCH(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return schemecode.New(env).Compile(prog)
			})
		})
	}
}
