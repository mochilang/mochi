//go:build slow

package types_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func runMochiCheck(src string) ([]byte, error) {
	data, err := os.ReadFile(src)
	if err != nil {
		return nil, err
	}
	driver := fmt.Sprintf("import \"core/mochi/types/check.mochi\" as tc\nprint(tc.checkString(%q))", string(data))
	prog, err := parser.ParseString(driver)
	if err != nil {
		return nil, fmt.Errorf("driver parse: %w", err)
	}
	env := types.NewEnv(nil)
	root, _ := mod.FindRoot(filepath.Dir(src))
	interp := interpreter.New(prog, env, root)
	var out bytes.Buffer
	interp.Env().SetWriter(&out)
	if err := interp.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestMochiTypeCheckerValid(t *testing.T) {
	golden.Run(t, "tests/mochi_in_mochi/types/valid", ".mochi", ".golden", func(src string) ([]byte, error) {
		return runMochiCheck(src)
	})
}

func TestMochiTypeCheckerErrors(t *testing.T) {
	golden.Run(t, "tests/mochi_in_mochi/types/errors", ".mochi", ".err", func(src string) ([]byte, error) {
		return runMochiCheck(src)
	})
}
