//go:build archived && slow

package cppcode_test

import (
	"fmt"
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_TPCDS(t *testing.T) {
	if _, err := cppcode.EnsureCPP(); err != nil {
		t.Skipf("C++ compiler not installed: %v", err)
	}
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCDS(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return cppcode.New(env).Compile(prog)
		})
	}
}
