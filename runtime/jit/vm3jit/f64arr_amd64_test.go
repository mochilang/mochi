//go:build amd64

package vm3jit_test

import (
	"math"
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestF64ArrayJITGetSetAMD64 exercises the Phase 6.3.4.n.3 AMD64
// lowering for the OpNewF64Array (pre-alloc skip) + OpF64ArraySetF64 +
// OpF64ArrayGetF64 round-trip on a cell-bank+f64 fn. The shape mirrors
// the ARM64 TestF64ArrayJITGetSet so the JIT path is bit-for-bit
// comparable across backends.
func TestF64ArrayJITGetSetAMD64(t *testing.T) {
	cs := []vm3.Cell{
		vm3.CFloat(1.5),
		vm3.CFloat(-2.25),
		vm3.CFloat(0.0),
		vm3.CFloat(math.Inf(1)),
		vm3.CFloat(math.Inf(-1)),
	}
	wantSum := 1.5 + -2.25 + 0.0 + math.Inf(1) + math.Inf(-1) // NaN

	fn := &vm3.Function{
		Name:        "f64arr_amd64_round_trip",
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
		t.Fatalf("f64arr_amd64_round_trip did not compile (JITCode is nil); "+
			"NumRegsCell=%d, NumRegsF64=%d, JITPreAllocF64ArrPrefix=%d",
			fn.NumRegsCell, fn.NumRegsF64, fn.JITPreAllocF64ArrPrefix)
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

// TestF64ArrayJITLenAMD64 mirrors the ARM64 TestF64ArrayJITLen for the
// AMD64 OpF64ArrayLenI64 lowering. The fn has no f64 regs so the cap is
// the cell-only 8; this also doubles as a check that LenI64 works on a
// cell-bank fn without the f64 base in R12.
func TestF64ArrayJITLenAMD64(t *testing.T) {
	const want = 7
	fn := &vm3.Function{
		Name:        "f64arr_amd64_len",
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
		t.Fatalf("f64arr_amd64_len did not compile (JITCode is nil)")
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

// TestSpectralNormJITAMD64 exercises the AMD64 cell-bank+f64 admission
// for the corpus.SpectralNorm shape. The fn has NumRegsI64=5,
// NumRegsF64=5, NumRegsCell=2 — fits the Phase 6.3.4.n.3 cap of
// i64<=6 when both Cell and F64 banks are present. The driver runs
// through the interp (it's a single-fn program); JITCallFn dispatches
// to the JIT'd entry. The result is checked against the Go oracle.
func TestSpectralNormJITAMD64(t *testing.T) {
	const n = int64(64)
	prog := corpus.SpectralNorm.Build(n)
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	fn := prog.Funcs[prog.Entry]
	if fn.JITCode == nil {
		t.Fatalf("spectral_norm did not JIT-compile on AMD64: "+
			"NumRegsI64=%d NumRegsF64=%d NumRegsCell=%d",
			fn.NumRegsI64, fn.NumRegsF64, fn.NumRegsCell)
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(fn, []int64{n})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	want := corpus.ExpectSpectralNorm(n)
	if math.Abs(got.Float()-want)/want > 1e-12 {
		t.Fatalf("spectral_norm(%d): got %.17g, want %.17g (delta %g)",
			n, got.Float(), want, got.Float()-want)
	}
}
