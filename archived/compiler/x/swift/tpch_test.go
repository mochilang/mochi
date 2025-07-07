//go:build archived && slow

package swiftcode_test

import (
	"testing"

	swiftcode "mochi/archived/x/swift"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_TPCH(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return swiftcode.New(env).Compile(prog)
			})
		})
	}
}
