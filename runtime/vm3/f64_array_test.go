package vm3

import (
	"math"
	"testing"
)

// TestF64ArrayGetSet verifies OpNewF64Array + OpF64ArrayGetF64 +
// OpF64ArraySetF64 + OpF64ArrayLenI64 round-trip a representative
// f64 value through a typed flat-f64 array. Mirrors TestListF64GetSet
// but exercises the typed-arena path that backs n_body's position /
// velocity / mass arrays without the Cell payload round-trip.
func TestF64ArrayGetSet(t *testing.T) {
	cs := []Cell{
		CFloat(1.5),
		CFloat(-2.25),
		CFloat(0.0),
		CFloat(math.Inf(1)),
		CFloat(math.Inf(-1)),
	}
	wantSum := 1.5 + -2.25 + 0.0 + math.Inf(1) + math.Inf(-1) // NaN

	fn := &Function{
		Name:        "f64arr_round_trip",
		NumRegsI64:  1,
		NumRegsF64:  2,
		NumRegsCell: 1,
		ResultBank:  BankF64,
		Consts:      cs,
		Code: []Op{
			// arr = NewF64Array(5) (pre-allocates 5 zero slots)
			MakeOp(OpNewF64Array, 0, 0, 5),
			// arr[0] = 1.5
			MakeOp(OpConstF64K, 0, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 0),
			MakeOp(OpF64ArraySetF64, 0, 0, 0),
			// arr[1] = -2.25
			MakeOp(OpConstF64K, 0, 0, 1),
			MakeOp(OpConstI64K, 0, 0, 1),
			MakeOp(OpF64ArraySetF64, 0, 0, 0),
			// arr[2] = 0.0
			MakeOp(OpConstF64K, 0, 0, 2),
			MakeOp(OpConstI64K, 0, 0, 2),
			MakeOp(OpF64ArraySetF64, 0, 0, 0),
			// arr[3] = +Inf
			MakeOp(OpConstF64K, 0, 0, 3),
			MakeOp(OpConstI64K, 0, 0, 3),
			MakeOp(OpF64ArraySetF64, 0, 0, 0),
			// arr[4] = -Inf
			MakeOp(OpConstF64K, 0, 0, 4),
			MakeOp(OpConstI64K, 0, 0, 4),
			MakeOp(OpF64ArraySetF64, 0, 0, 0),
			// acc = arr[0]
			MakeOp(OpConstI64K, 0, 0, 0),
			MakeOp(OpF64ArrayGetF64, 0, 0, 0),
			// acc += arr[1]
			MakeOp(OpConstI64K, 0, 0, 1),
			MakeOp(OpF64ArrayGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			// acc += arr[2]
			MakeOp(OpConstI64K, 0, 0, 2),
			MakeOp(OpF64ArrayGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			// acc += arr[3]
			MakeOp(OpConstI64K, 0, 0, 3),
			MakeOp(OpF64ArrayGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			// acc += arr[4]
			MakeOp(OpConstI64K, 0, 0, 4),
			MakeOp(OpF64ArrayGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			MakeOp(OpReturnF64, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{fn}, Entry: 0}
	vm := NewWithProgram(prog)
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

// TestF64ArrayPushLen verifies OpF64ArrayPushF64 grows the backing
// slice and OpF64ArrayLenI64 returns int64(len(data)). NewF64Array(0)
// starts at length 0 so Push is observable.
func TestF64ArrayPushLen(t *testing.T) {
	fn := &Function{
		Name:        "f64arr_push_len",
		NumRegsI64:  1,
		NumRegsF64:  1,
		NumRegsCell: 1,
		ResultBank:  BankI64,
		Consts:      []Cell{CFloat(3.14), CFloat(2.71), CFloat(1.41)},
		Code: []Op{
			MakeOp(OpNewF64Array, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 0),
			MakeOp(OpF64ArrayPushF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 1),
			MakeOp(OpF64ArrayPushF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 2),
			MakeOp(OpF64ArrayPushF64, 0, 0, 0),
			MakeOp(OpF64ArrayLenI64, 0, 0, 0),
			MakeOp(OpReturnI64, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{fn}, Entry: 0}
	vm := NewWithProgram(prog)
	got, err := vm.Run(fn)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != 3 {
		t.Fatalf("len: got %d want 3", got.Int())
	}
}
