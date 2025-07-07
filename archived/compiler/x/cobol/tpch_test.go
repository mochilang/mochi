//go:build archived && slow

package cobolcode_test

import (
	"fmt"
	"testing"

	cobolcode "mochi/archived/x/cobol"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_TPCH(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cobolcode.New(env).Compile(prog)
		})
	}
}
