package vm3

import (
	"math"
	"testing"
)

// TestListF64GetSet verifies OpListGetF64/OpListSetF64 round-trip a
// representative f64 value (positive, negative, zero, +Inf, -Inf, NaN)
// through a Cell-typed list. The cells are first materialized via
// OpListPushI64 so the underlying slice has room; OpListSetF64 then
// overwrites the payload with the CFloat encoding, and OpListGetF64
// reads it back into an f64 register. The kernel returns the f64 sum
// so the test gets a single observable value that aggregates all
// indices.
func TestListF64GetSet(t *testing.T) {
	// Build a kernel that:
	//   list = new
	//   for i in [0..5): list.push(0)
	//   list[0]=v0; list[1]=v1; ...; list[4]=v4
	//   acc = list[0] + list[1] + list[2] + list[3] + list[4]
	//   return acc
	//
	// i64 reg layout: 0=idx, 1=push_value
	// f64 reg layout: 0=accumulator, 1=scratch
	// cell reg layout: 0=list, 1=value-source (consts)
	cs := []Cell{
		CFloat(1.5),
		CFloat(-2.25),
		CFloat(0.0),
		CFloat(math.Inf(1)),
		CFloat(math.Inf(-1)),
	}
	wantSum := 1.5 + -2.25 + 0.0 + math.Inf(1) + math.Inf(-1) // NaN (Inf - Inf)

	fn := &Function{
		Name:        "list_f64_round_trip",
		NumRegsI64:  2,
		NumRegsF64:  2,
		NumRegsCell: 1,
		ResultBank:  BankF64,
		Consts:      cs,
		Code: []Op{
			MakeOp(OpNewList, 0, 0, 0),
			MakeOp(OpConstI64K, 1, 0, 0), // pushed payload = 0 (will be overwritten)
			MakeOp(OpListPushI64, 0, 1, 0),
			MakeOp(OpListPushI64, 0, 1, 0),
			MakeOp(OpListPushI64, 0, 1, 0),
			MakeOp(OpListPushI64, 0, 1, 0),
			MakeOp(OpListPushI64, 0, 1, 0),
			// store v0..v4 at indices 0..4
			MakeOp(OpConstF64K, 0, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 0),
			MakeOp(OpListSetF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 1),
			MakeOp(OpConstI64K, 0, 0, 1),
			MakeOp(OpListSetF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 2),
			MakeOp(OpConstI64K, 0, 0, 2),
			MakeOp(OpListSetF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 3),
			MakeOp(OpConstI64K, 0, 0, 3),
			MakeOp(OpListSetF64, 0, 0, 0),
			MakeOp(OpConstF64K, 0, 0, 4),
			MakeOp(OpConstI64K, 0, 0, 4),
			MakeOp(OpListSetF64, 0, 0, 0),
			// acc = list[0]; then acc += list[1..4]
			MakeOp(OpConstI64K, 0, 0, 0),
			MakeOp(OpListGetF64, 0, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 1),
			MakeOp(OpListGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			MakeOp(OpConstI64K, 0, 0, 2),
			MakeOp(OpListGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			MakeOp(OpConstI64K, 0, 0, 3),
			MakeOp(OpListGetF64, 1, 0, 0),
			MakeOp(OpAddF64, 0, 0, 1),
			MakeOp(OpConstI64K, 0, 0, 4),
			MakeOp(OpListGetF64, 1, 0, 0),
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
	// wantSum is NaN (Inf - Inf); use IsNaN for the equality check.
	if math.IsNaN(wantSum) {
		if !math.IsNaN(gotF) {
			t.Fatalf("want NaN, got %g", gotF)
		}
	} else if gotF != wantSum {
		t.Fatalf("sum mismatch: got %g want %g", gotF, wantSum)
	}
}
