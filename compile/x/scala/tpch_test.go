//go:build slow

package scalacode_test

import (
	"testing"

	scalacode "mochi/compile/x/scala"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestScalaCompiler_TPCH(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return scalacode.New(env).Compile(prog)
	})
}
