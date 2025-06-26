//go:build slow

package ftncode_test

import (
	"testing"

	ftncode "mochi/compile/x/fortran"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFortranCompiler_TPCH(t *testing.T) {
	if _, err := ftncode.EnsureFortran(); err != nil {
		t.Skipf("fortran not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ftncode.New().Compile(prog)
	})
}
