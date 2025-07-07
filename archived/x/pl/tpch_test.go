//go:build archived && slow

package plcode_test

import (
	"fmt"
	"testing"

	plcode "mochi/archived/x/pl"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_TPCH(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return plcode.New(env).Compile(prog)
		})
	}
}
