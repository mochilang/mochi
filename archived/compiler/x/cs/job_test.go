//go:build archived && slow

package cscode_test

import (
	"fmt"
	parser "mochi/parser"
	"mochi/types"
	"testing"

	cscode "mochi/archived/x/cs"
	"mochi/archived/x/testutil"
)

func TestCSCompiler_JOB(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		query := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, query, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cscode.New(env).Compile(prog)
		})
	}
}
