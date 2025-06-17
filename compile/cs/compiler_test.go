package cscode_test

import (
	"bytes"
	"fmt"
	"testing"

	cscode "mochi/compile/cs"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/cs", ".mochi", ".cs.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := cscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
