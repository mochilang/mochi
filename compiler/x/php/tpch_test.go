//go:build slow

package phpcode_test

import (
	"fmt"
	"os/exec"
	"testing"

	phpcode "mochi/compiler/x/php"
	testutil "mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_TPCH(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return phpcode.New(env).Compile(prog)
		})
	}
}
