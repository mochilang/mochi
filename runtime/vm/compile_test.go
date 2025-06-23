package vm_test

import (
	"bytes"
	"fmt"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
)

func run(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("\u274c parse error: %w", err)
	}
	builtins := map[string]int{"print": 0, "neg": 1}
	comp, err := vm.Compile(prog, builtins)
	if err != nil {
		return nil, fmt.Errorf("\u274c compile error: %w", err)
	}
	m := vm.New(comp.Ops, comp.Consts, comp.NumRegs)
	var buf bytes.Buffer
	m.Register(func(args []vm.Value) (vm.Value, error) {
		fmt.Fprintln(&buf, args[0])
		return nil, nil
	})
	m.Register(func(args []vm.Value) (vm.Value, error) {
		return -args[0].(int), nil
	})
	if _, err := m.Run(); err != nil {
		return nil, fmt.Errorf("\u274c run error: %w", err)
	}
	return bytes.TrimSpace(buf.Bytes()), nil
}

func TestVMCompiler_Golden(t *testing.T) {
	golden.Run(t, "tests/compiler/vm", ".mochi", ".out", func(src string) ([]byte, error) {
		return run(src)
	})
}
