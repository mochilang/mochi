//go:build darwin && arm64

package vm3jit_test

import (
	"math"
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestF64ArrayJITGetSet exercises the Phase 6.3.4.j.5.b ARM64 lowering
// for OpNewF64Array (pre-alloc skip), OpF64ArraySetF64, OpF64ArrayGetF64,
// and OpF64ArrayLenI64. The kernel pre-allocates a 5-slot typed array,
// writes 5 representative f64 values (including +Inf/-Inf to lock in the
// raw-bits round trip), reads them back, sums via OpAddF64, and returns
// the sum. The shape mirrors vm3.TestF64ArrayGetSet so the JIT path is
// bit-for-bit comparable to the interpreter.
func TestF64ArrayJITGetSet(t *testing.T) {
	cs := []vm3.Cell{
		vm3.CFloat(1.5),
		vm3.CFloat(-2.25),
		vm3.CFloat(0.0),
		vm3.CFloat(math.Inf(1)),
		vm3.CFloat(math.Inf(-1)),
	}
	wantSum := 1.5 + -2.25 + 0.0 + math.Inf(1) + math.Inf(-1) // NaN

	fn := &vm3.Function{
		Name:        "f64arr_jit_round_trip",
		NumRegsI64:  1,
		NumRegsF64:  2,
		NumRegsCell: 1,
		ResultBank:  vm3.BankF64,
		Consts:      cs,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewF64Array, 0, 0, 5),
			vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),
			vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstF64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstF64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstF64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstF64K, 0, 0, 4),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 4),
			vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),
			vm3.MakeOp(vm3.OpF64ArrayGetF64, 0, 0, 0),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 0),
			vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 0),
			vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 0),
			vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 4),
			vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 0),
			vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
			vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0),
		},
	}
	prog := &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	if fn.JITCode == nil {
		t.Fatalf("f64arr_jit_round_trip did not compile (JITCode is nil); "+
			"NumRegsCell=%d, JITPreAllocF64ArrPrefix=%d", fn.NumRegsCell, fn.JITPreAllocF64ArrPrefix)
	}
	if fn.JITPreAllocF64ArrPrefix != 1 {
		t.Fatalf("JITPreAllocF64ArrPrefix: got %d want 1", fn.JITPreAllocF64ArrPrefix)
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.Run(fn)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	gotF := got.Float()
	if math.IsNaN(wantSum) {
		if !math.IsNaN(gotF) {
			t.Fatalf("want NaN, got %g", gotF)
		}
	} else if gotF != wantSum {
		t.Fatalf("sum mismatch: got %g want %g", gotF, wantSum)
	}
}

// TestF64ArrayJITLen exercises the Phase 6.3.4.j.5.b ARM64 lowering for
// OpF64ArrayLenI64. The kernel pre-allocates a typed array sized via op.C
// and immediately returns its length so the JIT path covers the cold
// 5-inst form (UXTW + MOV stride + MUL + ADD + LDR Wd).
func TestF64ArrayJITLen(t *testing.T) {
	const want = 7
	fn := &vm3.Function{
		Name:        "f64arr_jit_len",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewF64Array, 0, 0, want),
			vm3.MakeOp(vm3.OpF64ArrayLenI64, 0, 0, 0),
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
		},
	}
	prog := &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	if fn.JITCode == nil {
		t.Fatalf("f64arr_jit_len did not compile (JITCode is nil)")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.Run(fn)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != int64(want) {
		t.Fatalf("len: got %d want %d", got.Int(), want)
	}
}
