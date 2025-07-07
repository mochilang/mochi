//go:build archived && slow

package zigcode_test

import (
	"fmt"
	"testing"

	"mochi/archived/x/testutil"
	zigcode "mochi/archived/x/zig"
	"mochi/parser"
	"mochi/types"
)

func TestZigCompiler_JOB(t *testing.T) {
	if _, err := zigcode.EnsureZig(); err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return zigcode.New(env).Compile(prog)
		})
	}
}
