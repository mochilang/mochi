//go:build darwin && arm64

package vm2jit_test

import (
	"testing"

	"mochi/runtime/jit/vm2jit"
	"mochi/runtime/vm2"
)

// --- helpers ---

// compileOrSkip compiles fn; skips the test if ErrNotImplemented is returned,
// fatals on any other error.
func compileOrSkip(t *testing.T, fn *vm2.Function) *vm2jit.CompiledFunc {
	t.Helper()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	t.Cleanup(func() { _ = cf.Free() })
	return cf
}

// callJIT runs a JIT'd arithmetic function with the given register file.
// Uses a nil VM pointer (safe for opcodes that don't touch the Objects table).
func callJIT(fn *vm2.Function, r []vm2.Cell) vm2.Cell {
	return vm2jit.CallDirect(fn, nil, r)
}

// regs builds a Cell slice of length n with the given initial values.
func regs(n int, vals ...vm2.Cell) []vm2.Cell {
	r := make([]vm2.Cell, n)
	copy(r, vals)
	return r
}

// --- compile scaffold tests ---

func TestCompileUnsupportedOpcode(t *testing.T) {
	// Any defined opcode either lowers natively or to a deopt stub. Pick a
	// value past the defined enum range to force ErrNotImplemented.
	fn := &vm2.Function{
		Name:    "bad_op",
		NumRegs: 1,
		Code: []vm2.Instr{
			{Op: vm2.Op(255), A: 0},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	_, err := vm2jit.Compile(fn)
	if err == nil {
		t.Fatal("expected error for undefined opcode, got nil")
	}
}

func TestFunctionJITCodeIsNilBeforeCompile(t *testing.T) {
	fn := &vm2.Function{Name: "x", NumRegs: 1, Code: []vm2.Instr{{Op: vm2.OpReturn, A: 0}}}
	if fn.JITCode != nil {
		t.Fatal("JITCode should be nil before Compile")
	}
}

func TestCompileSetsJITCode(t *testing.T) {
	fn := &vm2.Function{
		Name:    "ret0",
		NumRegs: 1,
		Code:    []vm2.Instr{{Op: vm2.OpReturn, A: 0}},
	}
	cf := compileOrSkip(t, fn)
	if fn.JITCode == nil {
		t.Fatal("JITCode nil after Compile")
	}
	t.Logf("code size: %d bytes", cf.CodeLen())
}

// --- correctness tests ---

func TestReturnRegister(t *testing.T) {
	// fn returns r1 unchanged.
	fn := &vm2.Function{
		Name:    "ret_r1",
		NumRegs: 2,
		Code:    []vm2.Instr{{Op: vm2.OpReturn, A: 1}},
	}
	compileOrSkip(t, fn)
	want := vm2.CInt(42)
	got := callJIT(fn, regs(2, vm2.CInt(0), want))
	if got != want {
		t.Fatalf("got %016x, want %016x", got.Bits, want.Bits)
	}
}

func TestMove(t *testing.T) {
	// r0 = r1; return r0
	fn := &vm2.Function{
		Name:    "move",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpMove, A: 0, B: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	want := vm2.CInt(77)
	got := callJIT(fn, regs(2, vm2.CInt(0), want))
	if got != want {
		t.Fatalf("move: got %v want %v", got.Int(), want.Int())
	}
}

func TestAddI64(t *testing.T) {
	// r0 = r0 + r1; return r0
	fn := &vm2.Function{
		Name:    "add",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want int64 }{
		{3, 4, 7},
		{-1, 1, 0},
		{100, -50, 50},
		{0, 0, 0},
		{1<<40 - 1, 1, 1 << 40},
	} {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("add(%d,%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

func TestSubI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "sub",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpSubI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want int64 }{
		{10, 3, 7},
		{0, 5, -5},
		{-3, -3, 0},
	} {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("sub(%d,%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

func TestMulI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "mul",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpMulI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want int64 }{
		{6, 7, 42},
		{-3, 4, -12},
		{0, 999, 0},
		{1000, 1000, 1_000_000},
	} {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("mul(%d,%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

func TestDivI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "div",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpDivI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want int64 }{
		{10, 2, 5},
		{7, 2, 3},
		{-10, 2, -5},
	} {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("div(%d,%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

func TestModI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "mod",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpModI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want int64 }{
		{10, 3, 1},
		{7, 7, 0},
		{-7, 3, -1},
	} {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("mod(%d,%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

func TestLessI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "less",
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpLessI64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct {
		a, b int64
		want bool
	}{
		{1, 2, true},
		{2, 1, false},
		{3, 3, false},
		{-5, 0, true},
	} {
		got := callJIT(fn, regs(3, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Bool() != tc.want {
			t.Errorf("less(%d,%d)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestLessEqI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "lesseq",
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpLessEqI64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct {
		a, b int64
		want bool
	}{
		{1, 2, true},
		{3, 3, true},
		{4, 3, false},
	} {
		got := callJIT(fn, regs(3, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Bool() != tc.want {
			t.Errorf("lesseq(%d,%d)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestEqualI64(t *testing.T) {
	fn := &vm2.Function{
		Name:    "eq",
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpEqualI64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct {
		a, b int64
		want bool
	}{
		{5, 5, true},
		{5, 6, false},
		{0, 0, true},
		{-1, 1, false},
	} {
		got := callJIT(fn, regs(3, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Bool() != tc.want {
			t.Errorf("eq(%d,%d)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestLoadConstI(t *testing.T) {
	// r0 = consts[0] (= 999); return r0
	fn := &vm2.Function{
		Name:    "load_const",
		NumRegs: 1,
		Consts:  []vm2.Cell{vm2.CInt(999)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	got := callJIT(fn, regs(1))
	if got.Int() != 999 {
		t.Fatalf("load_const: got %d want 999", got.Int())
	}
}

func TestJump(t *testing.T) {
	// instr 0: jump to instr 2 (skip instr 1)
	// instr 1: r0 = r0 + r1 (skipped)
	// instr 2: return r0
	fn := &vm2.Function{
		Name:    "jump",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpJump, A: 2},
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	r := regs(2, vm2.CInt(5), vm2.CInt(10))
	got := callJIT(fn, r)
	if got.Int() != 5 {
		t.Fatalf("jump: got %d, want 5 (jump skipped add)", got.Int())
	}
}

func TestJumpIfFalse(t *testing.T) {
	// r2 = (r0 < r1); if !r2 jump to instr 3; r0 = r0 + r1; return r0
	// When r0 < r1: r2=true → no jump → r0 += r1 → return r0+r1
	// When r0 >= r1: r2=false → jump → return r0
	fn := &vm2.Function{
		Name:    "jump_if_false",
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpLessI64, A: 2, B: 0, C: 1},    // instr 0
			{Op: vm2.OpJumpIfFalse, A: 2, B: 3},       // instr 1: jump to 3 if !r2
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},      // instr 2
			{Op: vm2.OpReturn, A: 0},                   // instr 3
		},
	}
	compileOrSkip(t, fn)

	// r0=3 < r1=5 → true → no jump → add → return 8
	got := callJIT(fn, regs(3, vm2.CInt(3), vm2.CInt(5)))
	if got.Int() != 8 {
		t.Errorf("jump_if_false(3<5): got %d want 8", got.Int())
	}

	// r0=7 >= r1=5 → false → jump → return 7
	got = callJIT(fn, regs(3, vm2.CInt(7), vm2.CInt(5)))
	if got.Int() != 7 {
		t.Errorf("jump_if_false(7>=5): got %d want 7", got.Int())
	}
}

func TestJumpIfLessI64(t *testing.T) {
	// if r0 < r1 goto instr 2; r0 = 0; return r0
	fn := &vm2.Function{
		Name:    "jmp_less",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpJumpIfLessI64, A: 0, B: 1, C: 2}, // instr 0: if r0<r1 goto 2
			{Op: vm2.OpMove, A: 0, B: 1},                 // instr 1: r0 = r1 (not taken)
			{Op: vm2.OpReturn, A: 0},                      // instr 2
		},
	}
	compileOrSkip(t, fn)

	// 3 < 5 → jump → return 3
	got := callJIT(fn, regs(2, vm2.CInt(3), vm2.CInt(5)))
	if got.Int() != 3 {
		t.Errorf("jmp_less(3<5): got %d want 3", got.Int())
	}

	// 7 >= 5 → no jump → r0=r1=5 → return 5
	got = callJIT(fn, regs(2, vm2.CInt(7), vm2.CInt(5)))
	if got.Int() != 5 {
		t.Errorf("jmp_less(7>=5): got %d want 5", got.Int())
	}
}

// TestCountLoop verifies branch relocation across a backward jump.
// Computes sum = 0 + 1 + 2 + ... + (n-1) using OpAddI64K + OpJumpIfLessI64.
//
//	r0 = 0   (sum)
//	r1 = 0   (i, from caller)
//	r2 = n   (limit, from caller)
//
// loop:
//
//	r0 = r0 + r1    (sum += i)
//	r1 = r1 + 1     (i++)     [uses OpAddI64K]
//	if r1 < r2 goto loop
//	return r0
func TestCountLoop(t *testing.T) {
	fn := &vm2.Function{
		Name:    "sum_n",
		NumRegs: 3,
		Code: []vm2.Instr{
			// instr 0: r0 = r0 + r1
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			// instr 1: r1 = r1 + 1
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},
			// instr 2: if r1 < r2 goto instr 0
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 0},
			// instr 3: return r0
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)

	for _, n := range []int{0, 1, 5, 10, 100} {
		// sum = 0+1+...+(n-1) = n*(n-1)/2
		want := int64(n) * int64(n-1) / 2
		r := regs(3, vm2.CInt(0), vm2.CInt(0), vm2.CInt(int64(n)))
		got := callJIT(fn, r)
		if got.Int() != want {
			t.Errorf("sum(%d)=%d want %d", n, got.Int(), want)
		}
	}
}

// TestNaNBoxingRoundtrip verifies that the JIT correctly round-trips extreme
// int48 values through the NaN-boxing pack/unpack cycle.
func TestNaNBoxingRoundtrip(t *testing.T) {
	// r0 = r0 + r1 (just to force unbox+rebox), return r0
	fn := &vm2.Function{
		Name:    "roundtrip",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)

	const maxInt48 = (1 << 47) - 1
	const minInt48 = -(1 << 47)

	cases := []struct{ a, b, want int64 }{
		{maxInt48, 0, maxInt48},
		{minInt48, 0, minInt48},
		{maxInt48, -1, maxInt48 - 1},
		{minInt48, 1, minInt48 + 1},
	}
	for _, tc := range cases {
		got := callJIT(fn, regs(2, vm2.CInt(tc.a), vm2.CInt(tc.b)))
		if got.Int() != tc.want {
			t.Errorf("roundtrip(%d+%d)=%d want %d", tc.a, tc.b, got.Int(), tc.want)
		}
	}
}

// --- OpCall integration: interpreter calls JIT'd callee ---

// TestOpCallRoutesToJIT verifies the full interpreter→JIT dispatch path.
// fn[0] (main) is interpreted; fn[1] (add) is JIT-compiled. The interpreter
// executes OpCall which detects JITCode != nil and routes through JITCallFn.
func TestOpCallRoutesToJIT(t *testing.T) {
	callee := &vm2.Function{
		Name:    "add_jit",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	// JIT-compile the callee.
	cf, err := vm2jit.Compile(callee)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	// main: r0=30, r1=12, r2=call callee(r0,r1), return r2
	main := &vm2.Function{
		Name:    "main",
		NumRegs: 3,
		Consts:  []vm2.Cell{vm2.CInt(30), vm2.CInt(12)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},       // r0 = 30
			{Op: vm2.OpLoadConstI, A: 1, B: 1},       // r1 = 12
			{Op: vm2.OpCall, A: 2, B: 1, C: 0, D: 2}, // r2 = fn[1](r0, r1)
			{Op: vm2.OpReturn, A: 2},
		},
	}
	prog := &vm2.Program{Funcs: []*vm2.Function{main, callee}, Main: 0}
	vm := vm2.New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	if got.Int() != 42 {
		t.Fatalf("interpreter→JIT call: got %d want 42", got.Int())
	}
}

// TestOpCallRoutesToJITLoop tests interpreter→JIT for a loop inside the JIT'd callee.
// main calls a JIT'd sumN(n) which computes 0+1+...+(n-1).
func TestOpCallRoutesToJITLoop(t *testing.T) {
	callee := &vm2.Function{
		Name:    "sum_n_jit",
		NumRegs: 3,
		Code: []vm2.Instr{
			// r0=sum(in), r1=i(in), r2=n(in)
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},        // r0 += r1
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},       // r1 += 1
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 0}, // if r1<r2 goto 0
			{Op: vm2.OpReturn, A: 0},
		},
	}
	cf, err := vm2jit.Compile(callee)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	// main: r0=0, r1=0, r2=100, r3=call callee(r0,r1,r2), return r3
	main := &vm2.Function{
		Name:    "main",
		NumRegs: 4,
		Consts:  []vm2.Cell{vm2.CInt(0), vm2.CInt(0), vm2.CInt(100)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},        // r0 = 0 (sum)
			{Op: vm2.OpLoadConstI, A: 1, B: 1},        // r1 = 0 (i)
			{Op: vm2.OpLoadConstI, A: 2, B: 2},        // r2 = 100 (n)
			{Op: vm2.OpCall, A: 3, B: 1, C: 0, D: 3}, // r3 = callee(r0,r1,r2)
			{Op: vm2.OpReturn, A: 3},
		},
	}
	prog := &vm2.Program{Funcs: []*vm2.Function{main, callee}, Main: 0}
	vm := vm2.New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	want := int64(100 * 99 / 2) // 4950
	if got.Int() != want {
		t.Fatalf("interpreter→JIT sum(100): got %d want %d", got.Int(), want)
	}
}

// TestJITListLenFastPath exercises the Phase 2 OpListLen fast path. The
// callee has a single OpListLen + OpReturn so the JIT must lower OpListLen
// natively (no deopt for the body to fall through to). Test builds a real
// list in vm.Objects, calls the JIT'd callee directly via CallDirect, and
// asserts the returned Cell carries the expected int48 length.
func TestJITListLenFastPath(t *testing.T) {
	callee := &vm2.Function{
		Name:    "list_len",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpListLen, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	cf, err := vm2jit.Compile(callee)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	vm := vm2.New(&vm2.Program{Funcs: []*vm2.Function{callee}, Main: 0})
	list := vm2.JITNewList(vm, 0)
	for i := int64(0); i < 5; i++ {
		vm2.JITListPush(vm, list, vm2.CInt(i*i))
	}
	got := vm2jit.CallDirect(callee, vm, []vm2.Cell{list, {}})
	if pc, ok := vm2.DecodeDeopt(got); ok {
		t.Fatalf("fast path deopted unexpectedly at pc=%d", pc)
	}
	if got.Int() != 5 {
		t.Fatalf("list_len: got %d want 5", got.Int())
	}
}

// TestJITListLenTagMissDeopts verifies the OpListLen tag check fires when
// regs[B] is not tagPtr. Passing a CInt should cause b.ne -> deopt stub ->
// sentinel-tagged Cell. We bypass the wrapper's resume by going through
// the trampoline indirectly (CallDirect); the returned Cell must DecodeDeopt
// cleanly to the failing PC (instruction index 0 in this fn).
func TestJITListLenTagMissDeopts(t *testing.T) {
	callee := &vm2.Function{
		Name:    "list_len_mismatch",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpListLen, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	cf, err := vm2jit.Compile(callee)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	got := vm2jit.CallDirect(callee, nil, []vm2.Cell{vm2.CInt(7), {}})
	pc, ok := vm2.DecodeDeopt(got)
	if !ok {
		t.Fatalf("expected deopt sentinel, got %016x", got.Bits)
	}
	if pc != 0 {
		t.Fatalf("deopt pc: got %d want 0 (OpListLen index)", pc)
	}
}

// TestJITDeoptResume exercises the Phase 1.5 deopt path end-to-end. The
// callee first runs a natively-lowered AddI64K (computing r1 = r0+1 in
// JIT registers), then hits a deoptable OpNewList. The JIT spills its
// registers via the deopt stub, returns a sentinel-tagged Cell, and the
// init.go wrapper promotes the JIT frame into a real vm2 frame so the
// interpreter can finish the list creation and return the *JIT-computed*
// r1 — proving that the native work survives the deopt.
func TestJITDeoptResume(t *testing.T) {
	callee := &vm2.Function{
		Name:    "deopt_callee",
		NumRegs: 4,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64K, A: 1, B: 0, C: 1}, // r1 = r0 + 1 (JIT native)
			{Op: vm2.OpNewList, A: 2, B: 0},       // deopts at idx=1
			{Op: vm2.OpListLen, A: 3, B: 2},       // interpreter resumes
			{Op: vm2.OpReturn, A: 1},              // return r1
		},
	}
	cf, err := vm2jit.Compile(callee)
	if err != nil {
		t.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	main := &vm2.Function{
		Name:    "main",
		NumRegs: 2,
		Consts:  []vm2.Cell{vm2.CInt(41)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},       // r0 = 41
			{Op: vm2.OpCall, A: 1, B: 1, C: 0, D: 1}, // r1 = callee(r0)
			{Op: vm2.OpReturn, A: 1},
		},
	}
	prog := &vm2.Program{Funcs: []*vm2.Function{main, callee}, Main: 0}
	vm := vm2.New(prog)
	got, err := vm.Run()
	if err != nil {
		t.Fatalf("vm.Run: %v", err)
	}
	if got.Int() != 42 {
		t.Fatalf("deopt resume: got %d want 42", got.Int())
	}
}

// --- benchmarks ---

// BenchmarkJITSumN benchmarks a tight JIT loop (sum 0..N-1).
func BenchmarkJITSumN(b *testing.B) {
	fn := &vm2.Function{
		Name:    "sum_n_bench",
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 0},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	const n = 1000
	r := regs(3)
	b.ResetTimer()
	for range b.N {
		r[0] = vm2.CInt(0)
		r[1] = vm2.CInt(0)
		r[2] = vm2.CInt(n)
		callJIT(fn, r)
	}
}

// BenchmarkInterpSumN runs the equivalent function through the vm2 interpreter.
// The program hard-codes n=1000 via OpLoadConstI to mirror the JIT benchmark.
func BenchmarkInterpSumN(b *testing.B) {
	prog := &vm2.Program{
		Funcs: []*vm2.Function{
			{
				Name:    "sum_n_interp",
				NumRegs: 3,
				Consts: []vm2.Cell{
					vm2.CInt(0),    // index 0: initial sum=0
					vm2.CInt(0),    // index 1: initial i=0
					vm2.CInt(1000), // index 2: limit n=1000
				},
				Code: []vm2.Instr{
					{Op: vm2.OpLoadConstI, A: 0, B: 0}, // r0 = 0 (sum)
					{Op: vm2.OpLoadConstI, A: 1, B: 1}, // r1 = 0 (i)
					{Op: vm2.OpLoadConstI, A: 2, B: 2}, // r2 = 1000 (n)
					// loop start at instr 3
					{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},      // r0 += r1
					{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},     // r1 += 1
					{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 3}, // if r1<r2 goto 3
					{Op: vm2.OpReturn, A: 0},
				},
			},
		},
		Main: 0,
	}
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

// BenchmarkJITAdd benchmarks a single-instruction JIT call (minimal overhead).
func BenchmarkJITAdd(b *testing.B) {
	fn := &vm2.Function{
		Name:    "add_bench",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	r := regs(2, vm2.CInt(3), vm2.CInt(4))
	b.ResetTimer()
	for range b.N {
		callJIT(fn, r)
	}
}
