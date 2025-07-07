//go:build archived && slow

package ftncode_test

import (
	"testing"

	ftncode "mochi/archived/x/fortran"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFortranCompiler_TPCH(t *testing.T) {
	if _, err := ftncode.EnsureFortran(); err != nil {
		t.Skipf("fortran not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return ftncode.New().Compile(prog)
			})
		})
	}
}
