//go:build slow

package swiftcode_test

import (
	"fmt"
	"testing"

	swiftcode "mochi/compile/x/swift"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_TPCDS(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	for i := 1; i <= 19; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCDS(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return swiftcode.New(env).Compile(prog)
		})
	}
}
