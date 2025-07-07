//go:build archived && slow

package stcode_test

import (
	"fmt"
	"testing"

	stcode "mochi/archived/x/st"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSTCompiler_TPCH(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return stcode.New(env).Compile(prog)
		})
	}
}
