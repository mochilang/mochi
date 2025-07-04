package go2mochi_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/tools/go2mochi"
	"mochi/types"
)

func TestGo2Mochi_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/go", ".go.out", ".mochi.out", func(src string) ([]byte, error) {
		code, err := go2mochi.Convert(src)
		if err != nil {
			return nil, fmt.Errorf("convert error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestGo2Mochi_Run(t *testing.T) {
	golden.Run(t, "tests/compiler/go", ".go.out", ".out", func(src string) ([]byte, error) {
		code, err := go2mochi.Convert(src)
		if err != nil {
			return nil, fmt.Errorf("convert error: %w", err)
		}
		prog, err := parser.ParseString(string(code))
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		out := &bytes.Buffer{}
		var r io.Reader = os.Stdin
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".go.out") + ".in"); err == nil {
			r = bytes.NewReader(data)
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		m := vm.NewWithIO(p, r, out)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("runtime error: %w", err)
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}
