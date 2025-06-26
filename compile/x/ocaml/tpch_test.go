//go:build slow

package mlcode_test

import (
	"testing"

	ocamlcode "mochi/compile/x/ocaml"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestOCamlCompiler_TPCH(t *testing.T) {
	if err := ocamlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocaml not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ocamlcode.New(env).Compile(prog)
	})
}
