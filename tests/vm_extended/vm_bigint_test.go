package vm_test

import (
	"bytes"
	"fmt"
	"os"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_BigInt(t *testing.T) {
	golden.Run(t, "tests/vm_extended/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		var out bytes.Buffer
		m := vm.New(p, &out)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("run error: %w", err)
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}

func TestVM_BigInt_IR(t *testing.T) {
	golden.Run(t, "tests/vm_extended/valid", ".mochi", ".ir.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		ir := p.Disassemble(string(data))
		return []byte(ir), nil
	})
}
