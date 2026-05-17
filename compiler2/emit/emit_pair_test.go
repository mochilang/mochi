package emit

import (
	"testing"

	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// TestEmitPairBasic exercises the MEP-37 §3.4 pair contract: a pair
// constructed from two ints reads back both slots and the values
// round-trip without loss.
func TestEmitPairBasic(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	p := b.NewPair(b.ConstI64(42), b.ConstI64(17))
	fst := b.PairFst(p, ir.TI64)
	snd := b.PairSnd(p, ir.TI64)
	s := b.AddI64(fst, snd)
	b.Ret(s)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	prog, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	r, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if r.Int() != 59 {
		t.Fatalf("got %d want 59", r.Int())
	}
}

// TestEmitPairNested verifies that pairs can carry pairs: the spec's
// motivating case is binary_trees, where every interior node is a pair
// of two pair children.
func TestEmitPairNested(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	inner := b.NewPair(b.ConstI64(3), b.ConstI64(4))
	outer := b.NewPair(inner, b.ConstI64(5))
	innerBack := b.PairFst(outer, ir.TPair)
	x := b.PairFst(innerBack, ir.TI64)
	y := b.PairSnd(innerBack, ir.TI64)
	z := b.PairSnd(outer, ir.TI64)
	s := b.AddI64(b.AddI64(x, y), z)
	b.Ret(s)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	prog, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	r, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if r.Int() != 12 {
		t.Fatalf("got %d want 12", r.Int())
	}
}
