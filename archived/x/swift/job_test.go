//go:build archived && slow

package swiftcode_test

import (
	"fmt"
	"testing"

	swiftcode "mochi/archived/x/swift"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_JOB(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return swiftcode.New(env).Compile(prog)
		})
	}
}
