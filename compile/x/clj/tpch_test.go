//go:build slow

package cljcode_test

import (
	"testing"

	cljcode "mochi/compile/x/clj"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestClojureCompiler_TPCH(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return cljcode.New(env).Compile(prog)
	})
}
