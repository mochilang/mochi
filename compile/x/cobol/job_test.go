//go:build slow

package cobolcode_test

import (
	"fmt"
	"os"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_JOB(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	os.Setenv("MOCHI_SKIP_COBFMT", "1")
	defer os.Unsetenv("MOCHI_SKIP_COBFMT")
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cobolcode.New(env).Compile(prog)
		})
	}
}
