//go:build archived && slow

package scalacode_test

import (
	"fmt"
	"testing"

	scalacode "mochi/archived/x/scala"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestScalaCompiler_TPCDS(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	for i := 10; i <= 49; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCDS(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return scalacode.New(env).Compile(prog)
			})
		})
	}
}
