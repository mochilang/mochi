//go:build slow

package swiftcode_test

import (
	"testing"

	swiftcode "mochi/compile/x/swift"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_TPCH(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return swiftcode.New(env).Compile(prog)
	})
}
