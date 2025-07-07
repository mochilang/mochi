//go:build archived && slow

package cscode_test

import (
	"fmt"
	"testing"

	cscode "mochi/archived/x/cs"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_TPCH(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cscode.New(env).Compile(prog)
		})
	}
}
