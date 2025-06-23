package vm

import "testing"

func TestArithmetic(t *testing.T) {
	consts := []Value{1, 2, 3}
	ops := []Instruction{
		{Op: OpLoadConst, A: 0, B: 0},
		{Op: OpLoadConst, A: 1, B: 1},
		{Op: OpLoadConst, A: 2, B: 2},
		{Op: OpMul, A: 3, B: 1, C: 2},
		{Op: OpAdd, A: 4, B: 0, C: 3},
		{Op: OpHalt, A: 4},
	}
	vm := New(ops, consts, 5)
	res, err := vm.Run()
	if err != nil {
		t.Fatalf("run error: %v", err)
	}
	if res != 7 {
		t.Fatalf("expected 7 got %v", res)
	}
}

func TestBuiltinCall(t *testing.T) {
	consts := []Value{5}
	ops := []Instruction{
		{Op: OpLoadConst, A: 0, B: 0},
		{Op: OpCall, A: 1, B: 0, C: 0},
		{Op: OpHalt, A: 1},
	}
	vm := New(ops, consts, 2)
	vm.Register(func(args []Value) (Value, error) {
		return -args[0].(int), nil
	})
	res, err := vm.Run()
	if err != nil {
		t.Fatalf("run error: %v", err)
	}
	if res != -5 {
		t.Fatalf("expected -5 got %v", res)
	}
}
