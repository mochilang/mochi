//go:build slow

package parser_test

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

func runMochiParser(src string) ([]byte, error) {
	data, err := os.ReadFile(src)
	if err != nil {
		return nil, err
	}
	driver := fmt.Sprintf("import \"core/mochi/parser/parser.mochi\" as mp\nprint(mp.parseString(%q))", string(data))
	prog, err := parser.ParseString(driver)
	if err != nil {
		return nil, fmt.Errorf("driver parse: %w", err)
	}
	env := types.NewEnv(nil)
	root, _ := mod.FindRoot(filepath.Dir(src))

	// no external functions are required

	interp := interpreter.New(prog, env, root)
	var out bytes.Buffer
	interp.Env().SetWriter(&out)
	if err := interp.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestMochiParser(t *testing.T) {
	golden.Run(t, "tests/mochi_in_mochi/parser", ".mochi", ".golden", func(src string) ([]byte, error) {
		return runMochiParser(src)
	})
}
