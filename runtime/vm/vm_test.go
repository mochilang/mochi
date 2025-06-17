package vm

import "testing"

func TestVMAdd(t *testing.T) {
	add := &Func{
		Name: "add",
		Regs: 3,
		Instrs: []Instr{
			{Op: OpAdd, A: 0, B: 1, C: 2},
			{Op: OpRet, A: 2},
		},
	}

	main := &Func{
		Name: "main",
		Regs: 3,
		Instrs: []Instr{
			{Op: OpLoadConst, C: 0, Arg: 1},
			{Op: OpLoadConst, C: 1, Arg: 2},
			{Op: OpCall, A: 0, B: 2, C: 2, Arg: add},
			{Op: OpRet, A: 2},
		},
	}

	v := New()
	res, err := v.Run(main)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}
	if res != float64(3) {
		t.Fatalf("expected 3, got %v", res)
	}
}
