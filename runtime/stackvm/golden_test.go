package stackvm

import (
	"bytes"
	"fmt"
	"os"
	"testing"

	"mochi/golden"
	"mochi/parser"
)

func TestStackVM_ValidPrograms(t *testing.T) {
	golden.Run(t, "tests/stackvm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		p, err := Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		var out bytes.Buffer
		v := New(p, &out)
		if err := v.Run(); err != nil {
			return nil, fmt.Errorf("run error: %w", err)
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}

func TestStackVM_IR(t *testing.T) {
	golden.Run(t, "tests/stackvm/valid", ".mochi", ".stack.ir.out", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		p, err := Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		ir := p.Disassemble(string(data))
		return []byte(ir), nil
	})
}
