//go:build archived && slow

package phpcode_test

import (
	"fmt"
	"testing"

	phpcode "mochi/archived/x/php"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_TPCH(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return phpcode.New(env).Compile(prog)
		})
	}
}
