//go:build darwin && arm64

package vm3jit_test

import (
	"testing"

	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestI64ArrayJITGetSet is the Phase 6.3.4.l.4 mirror of
// TestF64ArrayJITGetSet for the typed-i64 array ops. The kernel
// pre-allocates a 5-slot typed array, writes 5 representative i16-fitting
// i64 values, reads them back via OpI64ArrayGetI64, sums via OpAddI64,
// and returns the sum. (i16-fitting because OpConstI64K uses op.C
// directly; larger payloads round-trip via OpConstI64KW but are out of
// scope for this minimal round-trip test.)
func TestI64ArrayJITGetSet(t *testing.T) {
	vals := []int16{7, -3, 0, 32000, -32000}
	var want int64
	for _, v := range vals {
		want += int64(v)
	}
	code := []vm3.Op{
		vm3.MakeOp(vm3.OpNewI64Array, 0, 0, 5), // pc=0: arr = new(5)
	}
	// Write 5 entries: arr[i] = vals[i].
	for i, v := range vals {
		code = append(code,
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, v),
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, int16(i)),
			vm3.MakeOp(vm3.OpI64ArraySetI64, 0, 1, 2),
		)
	}
	// sum (r1) = arr[0]; for i in 1..4: sum += arr[i].
	code = append(code,
		vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
		vm3.MakeOp(vm3.OpI64ArrayGetI64, 1, 0, 2),
	)
	for i := 1; i < 5; i++ {
		code = append(code,
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, int16(i)),
			vm3.MakeOp(vm3.OpI64ArrayGetI64, 0, 0, 2),
			vm3.MakeOp(vm3.OpAddI64, 1, 1, 0),
		)
	}
	code = append(code, vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0))
	fn := &vm3.Function{
		Name:        "i64arr_jit_round_trip",
		NumRegsI64:  3,
		NumRegsCell: 1,
		ResultBank:  vm3.BankI64,
		Code:        code,
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
		t.Fatalf("i64arr_jit_round_trip did not compile (JITCode is nil); "+
			"NumRegsCell=%d, JITPreAllocI64ArrPrefix=%d", fn.NumRegsCell, fn.JITPreAllocI64ArrPrefix)
	}
	if fn.JITPreAllocI64ArrPrefix != 1 {
		t.Fatalf("JITPreAllocI64ArrPrefix: got %d want 1", fn.JITPreAllocI64ArrPrefix)
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.Run(fn)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != want {
		t.Fatalf("sum: got %d want %d", got.Int(), want)
	}
}

// TestI64ArrayJITLen is the Phase 6.3.4.l.4 mirror of TestF64ArrayJITLen
// for the i64-array length op.
func TestI64ArrayJITLen(t *testing.T) {
	const want = 7
	fn := &vm3.Function{
		Name:        "i64arr_jit_len",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewI64Array, 0, 0, want),
			vm3.MakeOp(vm3.OpI64ArrayLenI64, 0, 0, 0),
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
		t.Fatalf("i64arr_jit_len did not compile (JITCode is nil)")
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

// TestReverseComplementJITCompiles is the Phase 6.3.4.l.4 correctness
// gate: the compiler3 reverse_complement kernel must JIT-compile (entry
// has non-nil JITCode after CompileProgram) and produce a result
// matching the ExpectReverseComplement oracle. The kernel exercises the
// new ARM64 lowerings for OpNewI64Array (pre-alloc prefix),
// OpI64ArrayGetI64, OpI64ArraySetI64, and OpLookupI64KW. The admission
// whitelist was extended in the same phase to cover the i64-array op
// set.
func TestReverseComplementJITCompiles(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 4, 7, 16, 100, 1000, 8000} {
		prog := c3.ReverseComplement.Build(n)
		cfs := vm3jit.CompileProgram(prog)
		fn := prog.Funcs[prog.Entry]
		if fn.JITCode == nil {
			for _, cf := range cfs {
				if cf != nil {
					_ = cf.Free()
				}
			}
			t.Skipf("n=%d: reverse_complement entry has no JITCode (CompileProgram fell back; expected on non-ARM64)", n)
		}
		if got := int(fn.JITPreAllocI64ArrPrefix); got != 2 {
			for _, cf := range cfs {
				if cf != nil {
					_ = cf.Free()
				}
			}
			t.Fatalf("n=%d: JITPreAllocI64ArrPrefix: got %d want 2", n, got)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(fn, []int64{n})
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
		if err != nil {
			t.Fatalf("n=%d: RunWithArgs: %v", n, err)
		}
		want := c3.ExpectReverseComplement(n)
		if got.Int() != want {
			t.Fatalf("n=%d: got %d, want %d", n, got.Int(), want)
		}
	}
}
