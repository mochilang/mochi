//go:build archived && slow

package mlcode_test

import (
	"testing"

	ocamlcode "mochi/archived/x/ocaml"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestOCamlCompiler_TPCH(t *testing.T) {
	if err := ocamlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocaml not installed: %v", err)
	}
	queries := []string{"q1", "q2"}
	for _, q := range queries {
		q := q // capture
		t.Run(q, func(t *testing.T) {
			testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
				return ocamlcode.New(env).Compile(prog)
			})
		})
	}
}
