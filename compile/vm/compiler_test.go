package vmcode_test

import (
	"bytes"
	"fmt"
	"testing"

	vmcode "mochi/compile/vm"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
)

func TestVMCompiler_TwoSum(t *testing.T) {
	golden.Run(t, "tests/vm", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		code, regs, err := vmcode.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		m := vm.New(code, regs)
		var buf bytes.Buffer
		m.SetWriter(&buf)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("vm error: %w", err)
		}
		return bytes.TrimSpace(buf.Bytes()), nil
	})
}

func TestVMCompiler_IR(t *testing.T) {
	golden.Run(t, "tests/vm", ".mochi", ".ir.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		code, _, err := vmcode.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		return []byte(vm.Format(code)), nil
	})
}
