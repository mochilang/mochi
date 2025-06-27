//go:build slow

package rscode_test

import (
	"testing"

	rscode "mochi/compile/x/rust"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_JOB(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	testutil.CompileJOB(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return rscode.New(env).Compile(prog)
	})
}
