//go:build slow

package ocaml_test

import (
	"fmt"
	"os/exec"
	"testing"

	ocaml "mochi/compiler/x/ocaml"
	testutil "mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestOCamlCompiler_TPCH(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	for i := 1; i <= 5; i++ {
		q := fmt.Sprintf("q%d", i)
		testutil.CompileTPCH(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return ocaml.New(env).Compile(prog, "")
		})
	}
}
