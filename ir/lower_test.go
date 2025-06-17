package ir

import (
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
)

func compileSrc(src string) ([]vm.Instruction, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return Lower(prog)
}

func run(src string) (any, error) {
	ins, err := compileSrc(src)
	if err != nil {
		return nil, err
	}
	m := vm.New(ins)
	return m.Run()
}

func TestLetAndAdd(t *testing.T) {
	result, err := run("let x = 1 + 2")
	if err != nil {
		t.Fatal(err)
	}
	if result != nil {
		t.Fatalf("expected nil result, got %v", result)
	}
}

func TestExprReturn(t *testing.T) {
	ins, err := compileSrc("1 + 2")
	if err != nil {
		t.Fatal(err)
	}
	if len(ins) > 0 {
		ins[len(ins)-1].Op = vm.OpReturn
	} else {
		ins = append(ins, vm.Instruction{Op: vm.OpReturn})
	}
	m := vm.New(ins)
	val, err := m.Run()
	if err != nil {
		t.Fatal(err)
	}
	if val != 3 {
		t.Fatalf("expected 3 got %v", val)
	}
}
