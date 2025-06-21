package cloud_test

import (
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestTerraformExamples(t *testing.T) {
	golden.Run(t, "tests/cloud/terraform", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		interp := interpreter.New(prog, env, modRoot)
		out := &strings.Builder{}
		interp.Env().SetWriter(out)
		if err := interp.Run(); err != nil {
			return nil, err
		}
		return []byte(out.String()), nil
	})
}
