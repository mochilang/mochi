//go:build archived && slow

package rscode_test

import (
	"testing"

	rscode "mochi/archived/x/rust"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_TPCH(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return rscode.New(env).Compile(prog)
	})
}
