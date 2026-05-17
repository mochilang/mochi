package vm2

import "testing"

// runStrProg compiles a tiny hand-built program (single function, no
// calls) and returns the final cell. Used by every string op test so
// the assertions stay focused on opcode semantics, not framing.
func runStrProg(t *testing.T, numRegs int, strConsts [][]byte, code []Instr) (Cell, *VM) {
	t.Helper()
	fn := &Function{
		Name:      "test",
		NumParams: 0,
		NumRegs:   numRegs,
		Code:      code,
		StrConsts: strConsts,
	}
	prog := &Program{Funcs: []*Function{fn}, Main: 0}
	vm := New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	return got, vm
}

// strString returns the bytes backing a string Cell in either form.
func strString(vm *VM, c Cell) string {
	var buf [MaxInlineStr]byte
	return string(vm.strBytes(c, &buf))
}

func TestLoadStrK(t *testing.T) {
	// "hello" is 5 bytes, exactly MaxInlineStr — packed inline.
	got, vm := runStrProg(t, 1, [][]byte{[]byte("hello")}, []Instr{
		{Op: OpLoadStrK, A: 0, B: 0},
		{Op: OpReturn, A: 0},
	})
	if !got.IsStr() || !got.IsSStr() {
		t.Fatalf("want inline string cell, got %x", got.Bits)
	}
	if s := strString(vm, got); s != "hello" {
		t.Fatalf("want hello, got %q", s)
	}

	// 6-byte string overflows the inline budget; falls back to heap.
	got, vm = runStrProg(t, 1, [][]byte{[]byte("hello!")}, []Instr{
		{Op: OpLoadStrK, A: 0, B: 0},
		{Op: OpReturn, A: 0},
	})
	if !got.IsStr() || !got.IsPtr() {
		t.Fatalf("want heap string cell, got %x", got.Bits)
	}
	if string(vm.stringAt(got).bytes) != "hello!" {
		t.Fatalf("want hello!, got %q", vm.stringAt(got).bytes)
	}
}

func TestConcatStr(t *testing.T) {
	got, vm := runStrProg(t, 3, [][]byte{[]byte("foo"), []byte("bar")}, []Instr{
		{Op: OpLoadStrK, A: 0, B: 0},
		{Op: OpLoadStrK, A: 1, B: 1},
		{Op: OpConcatStr, A: 2, B: 0, C: 1},
		{Op: OpReturn, A: 2},
	})
	if s := strString(vm, got); s != "foobar" {
		t.Fatalf("want foobar, got %q", s)
	}
}

func TestLenStr(t *testing.T) {
	got, _ := runStrProg(t, 2, [][]byte{[]byte("héllo")}, []Instr{ // 6 bytes (é = 2)
		{Op: OpLoadStrK, A: 0, B: 0},
		{Op: OpLenStr, A: 1, B: 0},
		{Op: OpReturn, A: 1},
	})
	if got.Int() != 6 {
		t.Fatalf("want byte len 6, got %d", got.Int())
	}
}

func TestIndexStr(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "idx",
		NumRegs: 3,
		Code: []Instr{
			{Op: OpLoadConstI, A: 1, B: 0},
			{Op: OpLoadStrK, A: 0, B: 0},
			{Op: OpIndexStr, A: 2, B: 0, C: 1},
			{Op: OpReturn, A: 2},
		},
		Consts:    []Cell{CInt(1)},
		StrConsts: [][]byte{[]byte("abc")},
	}}, Main: 0}
	vm := New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if s := strString(vm, got); s != "b" {
		t.Fatalf("want \"b\", got %q", s)
	}
}

func TestIndexStrOOB(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "oob",
		NumRegs: 3,
		Code: []Instr{
			{Op: OpLoadConstI, A: 1, B: 0},
			{Op: OpLoadStrK, A: 0, B: 0},
			{Op: OpIndexStr, A: 2, B: 0, C: 1},
			{Op: OpReturn, A: 2},
		},
		Consts:    []Cell{CInt(5)},
		StrConsts: [][]byte{[]byte("abc")},
	}}, Main: 0}
	vm := New(prog)
	if _, err := vm.Run(); err == nil {
		t.Fatalf("want OOB error, got nil")
	}
}

func TestEqualStr(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "eq",
		NumRegs: 4,
		Code: []Instr{
			{Op: OpLoadStrK, A: 0, B: 0},
			{Op: OpLoadStrK, A: 1, B: 1}, // same bytes, different allocation
			{Op: OpLoadStrK, A: 2, B: 2},
			{Op: OpEqualStr, A: 3, B: 0, C: 1}, // true
			{Op: OpReturn, A: 3},
		},
		StrConsts: [][]byte{[]byte("foo"), []byte("foo"), []byte("bar")},
	}}, Main: 0}
	vm := New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if !got.Bool() {
		t.Fatalf("want true for foo==foo")
	}

	// Different strings.
	prog.Funcs[0].Code = []Instr{
		{Op: OpLoadStrK, A: 0, B: 0},
		{Op: OpLoadStrK, A: 1, B: 2},
		{Op: OpEqualStr, A: 3, B: 0, C: 1},
		{Op: OpReturn, A: 3},
	}
	vm = New(prog)
	got, err = vm.Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if got.Bool() {
		t.Fatalf("want false for foo==bar")
	}
}

func TestHashStrDeterministic(t *testing.T) {
	prog := &Program{Funcs: []*Function{{
		Name:    "h",
		NumRegs: 2,
		Code: []Instr{
			{Op: OpLoadStrK, A: 0, B: 0},
			{Op: OpHashStr, A: 1, B: 0},
			{Op: OpReturn, A: 1},
		},
		StrConsts: [][]byte{[]byte("the quick brown fox")},
	}}, Main: 0}
	vm := New(prog)
	a, err := vm.Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	vm = New(prog)
	b, err := vm.Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if a.Int() != b.Int() {
		t.Fatalf("hash not deterministic across runs: %d vs %d", a.Int(), b.Int())
	}
	if a.Int() == 0 {
		t.Fatalf("hash 0 is the sentinel; bytes should never hash to 0 after mapping")
	}
}

func TestStringHashMemoized(t *testing.T) {
	vm := New(&Program{Funcs: []*Function{{NumRegs: 1, Code: []Instr{{Op: OpReturn, A: 0}}}}, Main: 0})
	c := vm.newString([]byte("memoize"))
	s := vm.stringAt(c)
	if s.hash != 0 {
		t.Fatalf("hash should be lazy")
	}
	h1 := strHash(s)
	if s.hash != h1 {
		t.Fatalf("hash not memoized")
	}
	h2 := strHash(s)
	if h1 != h2 {
		t.Fatalf("hash unstable: %d vs %d", h1, h2)
	}
}
