//go:build archived && slow

package scalacode_test

import (
	"testing"

	scalacode "mochi/archived/x/scala"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestScalaCompiler_TPCH(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return scalacode.New(env).Compile(prog)
			})
		})
	}
}
