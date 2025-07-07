//go:build archived && slow

package dartcode_test

import (
	"fmt"
	"testing"

	dartcode "mochi/archived/x/dart"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_TPCDS(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	for i := 1; i <= 19; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCDS(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return dartcode.New(env).Compile(prog)
			})
		})
	}
}
