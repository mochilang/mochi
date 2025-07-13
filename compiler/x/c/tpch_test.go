//go:build slow

package ccode_test

import (
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCCompiler_TPCH(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ1Code(), nil
	})
	testutil.CompileTPCH(t, "q11", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ11Code(), nil
	})
}
