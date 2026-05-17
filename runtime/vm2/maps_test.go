package vm2

import "testing"

// runMapProg drives a hand-rolled Program through a fresh VM. Helper
// mirrors the lists tests so each case stays focused on opcode wiring.
func runMapProg(t *testing.T, p *Program) Cell {
	t.Helper()
	got, err := New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	return got
}

// TestNewMapEmpty checks that OpNewMap allocates and OpMapLen reports 0.
func TestNewMapEmpty(t *testing.T) {
	main := &Function{
		NumRegs: 2,
		Code: []Instr{
			{Op: OpNewMap, A: 0},
			{Op: OpMapLen, A: 1, B: 0},
			{Op: OpReturn, A: 1},
		},
	}
	got := runMapProg(t, &Program{Funcs: []*Function{main}, Main: 0})
	if got.Int() != 0 {
		t.Fatalf("len = %d, want 0", got.Int())
	}
}

// TestMapSetGetInt exercises OpMapSet / OpMapGet on int keys.
func TestMapSetGetInt(t *testing.T) {
	main := &Function{
		NumRegs: 4,
		Consts:  []Cell{CInt(42), CInt(7), CInt(99)},
		Code: []Instr{
			{Op: OpNewMap, A: 0},
			{Op: OpLoadConstI, A: 1, B: 0}, // k = 42
			{Op: OpLoadConstI, A: 2, B: 1}, // v = 7
			{Op: OpMapSet, A: 0, B: 1, C: 2},
			{Op: OpLoadConstI, A: 1, B: 2}, // k = 99 (missing)
			{Op: OpMapGet, A: 3, B: 0, C: 1},
			{Op: OpReturn, A: 3},
		},
	}
	got := runMapProg(t, &Program{Funcs: []*Function{main}, Main: 0})
	if !got.IsNull() {
		t.Fatalf("missing key = %#x, want null", got.Bits)
	}

	main.Code = []Instr{
		{Op: OpNewMap, A: 0},
		{Op: OpLoadConstI, A: 1, B: 0},
		{Op: OpLoadConstI, A: 2, B: 1},
		{Op: OpMapSet, A: 0, B: 1, C: 2},
		{Op: OpMapGet, A: 3, B: 0, C: 1},
		{Op: OpReturn, A: 3},
	}
	got = runMapProg(t, &Program{Funcs: []*Function{main}, Main: 0})
	if got.Int() != 7 {
		t.Fatalf("m[42] = %d, want 7", got.Int())
	}
}

// TestMapStringKeyAlias verifies that two distinct string Cells with
// the same bytes hit the same map entry (inline vs heap forms too).
func TestMapStringKeyAlias(t *testing.T) {
	// Constants: "hi" (inline) twice — distinct Cells but equal bytes.
	main := &Function{
		NumRegs:   4,
		Consts:    []Cell{CInt(11), CInt(22)},
		StrConsts: [][]byte{[]byte("hi"), []byte("hi")},
		Code: []Instr{
			{Op: OpNewMap, A: 0},
			{Op: OpLoadStrK, A: 1, B: 0},
			{Op: OpLoadConstI, A: 2, B: 0}, // 11
			{Op: OpMapSet, A: 0, B: 1, C: 2},
			{Op: OpLoadStrK, A: 1, B: 1}, // second "hi" cell
			{Op: OpMapGet, A: 3, B: 0, C: 1},
			{Op: OpReturn, A: 3},
		},
	}
	got := runMapProg(t, &Program{Funcs: []*Function{main}, Main: 0})
	if got.Int() != 11 {
		t.Fatalf("alias get = %d, want 11", got.Int())
	}
}

// TestMapHasAndDel checks OpMapHas before/after OpMapDel.
func TestMapHasAndDel(t *testing.T) {
	build := func(extra []Instr) *Program {
		code := []Instr{
			{Op: OpNewMap, A: 0},
			{Op: OpLoadConstI, A: 1, B: 0}, // k=1
			{Op: OpLoadConstI, A: 2, B: 1}, // v=5
			{Op: OpMapSet, A: 0, B: 1, C: 2},
		}
		code = append(code, extra...)
		return &Program{Funcs: []*Function{{
			NumRegs: 4,
			Consts:  []Cell{CInt(1), CInt(5)},
			Code:    code,
		}}, Main: 0}
	}

	got := runMapProg(t, build([]Instr{
		{Op: OpMapHas, A: 3, B: 0, C: 1},
		{Op: OpReturn, A: 3},
	}))
	if !got.Bool() {
		t.Fatalf("has before del = false, want true")
	}

	got = runMapProg(t, build([]Instr{
		{Op: OpMapDel, A: 0, B: 1},
		{Op: OpMapHas, A: 3, B: 0, C: 1},
		{Op: OpReturn, A: 3},
	}))
	if got.Bool() {
		t.Fatalf("has after del = true, want false")
	}
}
