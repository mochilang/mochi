package opt

import (
	"testing"

	"mochi/compiler2/ir"
)

func TestConstFoldArith(t *testing.T) {
	b := ir.NewBuilder("f", nil, ir.TI64)
	x := b.ConstI64(3)
	y := b.ConstI64(4)
	z := b.AddI64(x, y)
	b.Ret(z)
	fn := b.Function()

	if n := ConstFold(fn); n != 1 {
		t.Fatalf("ConstFold count = %d, want 1", n)
	}
	if fn.Values[z].Op != ir.OpConstI64 || fn.Values[z].Aux != 7 {
		t.Fatalf("z not folded to 7: %+v", fn.Values[z])
	}
}

func TestConstFoldCompare(t *testing.T) {
	b := ir.NewBuilder("f", nil, ir.TBool)
	x := b.ConstI64(2)
	y := b.ConstI64(5)
	z := b.LessI64(x, y)
	b.Ret(z)
	fn := b.Function()
	if n := ConstFold(fn); n != 1 {
		t.Fatalf("ConstFold count = %d, want 1", n)
	}
	if fn.Values[z].Op != ir.OpConstBool || fn.Values[z].Aux != 1 {
		t.Fatalf("z not folded to true: %+v", fn.Values[z])
	}
}

func TestDCERemovesDeadAdd(t *testing.T) {
	b := ir.NewBuilder("f", []ir.Type{ir.TI64}, ir.TI64)
	n := b.Param(0)
	dead := b.AddI64(n, n)
	_ = dead
	b.Ret(n)
	fn := b.Function()
	if got := DCE(fn); got != 1 {
		t.Fatalf("DCE removed = %d, want 1", got)
	}
	if err := ir.Verify(nil, fn); err != nil {
		t.Fatalf("Verify after DCE: %v", err)
	}
}

func TestDCEPreservesCall(t *testing.T) {
	const fIdx = 0
	b := ir.NewBuilder("f", nil, ir.TUnit)
	_ = b.Call(fIdx, ir.TI64) // unused but has effects
	b.Ret(-1)
	fn := b.Function()
	if got := DCE(fn); got != 0 {
		t.Fatalf("DCE removed call = %d, want 0", got)
	}
}

func TestDCECascades(t *testing.T) {
	b := ir.NewBuilder("f", []ir.Type{ir.TI64}, ir.TI64)
	n := b.Param(0)
	a := b.AddI64(n, n)
	c := b.MulI64(a, n) // dead; once removed, a becomes dead too
	_ = c
	b.Ret(n)
	fn := b.Function()
	if got := DCE(fn); got != 2 {
		t.Fatalf("DCE removed = %d, want 2 (cascading)", got)
	}
}
