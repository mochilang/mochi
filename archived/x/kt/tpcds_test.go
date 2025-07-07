//go:build archived && slow

package ktcode_test

import (
	"fmt"
	"testing"

	ktcode "mochi/archived/x/kt"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestKTCompiler_TPCDS(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	for i := 1; i <= 9; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCDS(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return ktcode.New(env).Compile(prog)
			})
		})
	}
}
