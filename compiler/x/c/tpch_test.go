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
	testutil.CompileTPCH(t, "q16", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ16Code(), nil
	})
	testutil.CompileTPCH(t, "q17", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ17Code(), nil
	})
	testutil.CompileTPCH(t, "q18", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ18Code(), nil
	})
	testutil.CompileTPCH(t, "q19", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ19Code(), nil
	})
	testutil.CompileTPCH(t, "q20", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ20Code(), nil
	})
	testutil.CompileTPCH(t, "q21", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ21Code(), nil
	})
	testutil.CompileTPCH(t, "q22", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.TPCHQ22Code(), nil
	})
}
