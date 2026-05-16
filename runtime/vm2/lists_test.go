package vm2

import "testing"

// runListProg compiles a tiny hand-built program and returns the final
// cell along with the VM so tests can poke the Objects table.
func runListProg(t *testing.T, numRegs int, consts []Cell, code []Instr) (Cell, *VM) {
	t.Helper()
	fn := &Function{
		Name:    "test",
		NumRegs: numRegs,
		Code:    code,
		Consts:  consts,
	}
	prog := &Program{Funcs: []*Function{fn}, Main: 0}
	vm := New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	return got, vm
}

func TestNewListAndLen(t *testing.T) {
	got, _ := runListProg(t, 2, nil, []Instr{
		{Op: OpNewList, A: 0, B: 4},
		{Op: OpListLen, A: 1, B: 0},
		{Op: OpReturn, A: 1},
	})
	if got.Int() != 0 {
		t.Fatalf("want empty list len 0, got %d", got.Int())
	}
}

func TestNewListCapHint(t *testing.T) {
	// Return the list ptr itself; the Objects entry survives the run
	// (only cleared on next Run), so we can inspect cap(data).
	got, vm := runListProg(t, 1, nil, []Instr{
		{Op: OpNewList, A: 0, B: 4},
		{Op: OpReturn, A: 0},
	})
	if !got.IsPtr() {
		t.Fatalf("want ptr cell, got %x", uint64(got))
	}
	l := vm.listAt(got)
	if cap(l.data) != 4 {
		t.Fatalf("want cap 4, got %d", cap(l.data))
	}
}

func TestListPushGet(t *testing.T) {
	got, _ := runListProg(t, 4, []Cell{CInt(42), CInt(0)}, []Instr{
		{Op: OpNewList, A: 0, B: 0},
		{Op: OpLoadConstI, A: 1, B: 0}, // 42
		{Op: OpListPush, A: 0, B: 1},
		{Op: OpLoadConstI, A: 2, B: 1}, // 0
		{Op: OpListGet, A: 3, B: 0, C: 2},
		{Op: OpReturn, A: 3},
	})
	if got.Int() != 42 {
		t.Fatalf("want 42, got %d", got.Int())
	}
}

func TestListSet(t *testing.T) {
	got, _ := runListProg(t, 5, []Cell{CInt(1), CInt(0), CInt(99)}, []Instr{
		{Op: OpNewList, A: 0, B: 2},
		{Op: OpLoadConstI, A: 1, B: 0}, // val=1
		{Op: OpListPush, A: 0, B: 1},
		{Op: OpLoadConstI, A: 2, B: 1}, // idx=0
		{Op: OpLoadConstI, A: 3, B: 2}, // val=99
		{Op: OpListSet, A: 0, B: 2, C: 3},
		{Op: OpListGet, A: 4, B: 0, C: 2},
		{Op: OpReturn, A: 4},
	})
	if got.Int() != 99 {
		t.Fatalf("want 99 after set, got %d", got.Int())
	}
}

func TestListGetOOB(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "oob",
		NumRegs: 3,
		Consts:  []Cell{CInt(5)},
		Code: []Instr{
			{Op: OpNewList, A: 0, B: 0},
			{Op: OpLoadConstI, A: 1, B: 0},
			{Op: OpListGet, A: 2, B: 0, C: 1},
			{Op: OpReturn, A: 2},
		},
	}}, Main: 0}
	vm := New(prog)
	if _, err := vm.Run(); err == nil {
		t.Fatalf("want OOB error, got nil")
	}
}

func TestListSetOOB(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "oob",
		NumRegs: 3,
		Consts:  []Cell{CInt(5)},
		Code: []Instr{
			{Op: OpNewList, A: 0, B: 0},
			{Op: OpLoadConstI, A: 1, B: 0},
			{Op: OpListSet, A: 0, B: 1, C: 1},
			{Op: OpReturn, A: 0},
		},
	}}, Main: 0}
	vm := New(prog)
	if _, err := vm.Run(); err == nil {
		t.Fatalf("want OOB error, got nil")
	}
}

func TestListPushAmortizedGrow(t *testing.T) {
	// Push 100 elements into a list created with zero capacity; verify
	// final length and that no error fires (i.e. the append path grows
	// correctly).
	consts := []Cell{CInt(1), CInt(100), CInt(0)}
	code := []Instr{
		{Op: OpNewList, A: 0, B: 0},
		{Op: OpLoadConstI, A: 1, B: 2}, // i = 0
		{Op: OpLoadConstI, A: 2, B: 1}, // n = 100
		{Op: OpLoadConstI, A: 3, B: 0}, // one = 1
		// pc=4 loop head: if i >= n goto end (pc=8)
		{Op: OpJumpIfGreaterEqI64, A: 1, B: 2, C: 8},
		// pc=5 push, pc=6 i++, pc=7 jump loop
		{Op: OpListPush, A: 0, B: 3},
		{Op: OpAddI64, A: 1, B: 1, C: 3},
		{Op: OpJump, A: 4},
		// pc=8 end: return len(list)
		{Op: OpListLen, A: 4, B: 0},
		{Op: OpReturn, A: 4},
	}
	got, _ := runListProg(t, 5, consts, code)
	if got.Int() != 100 {
		t.Fatalf("want 100, got %d", got.Int())
	}
}
