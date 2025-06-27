//go:build slow

package zigcode_test

import (
	"testing"

	"mochi/compile/x/testutil"
	zigcode "mochi/compile/x/zig"
	"mochi/parser"
	"mochi/types"
)

func TestZigCompiler_JOB(t *testing.T) {
	if _, err := zigcode.EnsureZig(); err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	testutil.CompileJOB(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return zigcode.New(env).Compile(prog)
	})
}
