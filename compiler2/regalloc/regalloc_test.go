package regalloc

import (
	"testing"

	"mochi/compiler2/ir"
)

func TestStraightLineReuse(t *testing.T) {
	// a = const 1
	// b = const 2
	// c = a + b   (last use of a, b)
	// d = c * c   (last use of c)
	// ret d
	b := ir.NewBuilder("f", nil, ir.TI64)
	a := b.ConstI64(1)
	c2 := b.ConstI64(2)
	c := b.AddI64(a, c2)
	d := b.MulI64(c, c)
	b.Ret(d)
	fn := b.Function()

	r := Run(fn, nil)
	if r.NumRegs == 0 {
		t.Fatalf("NumRegs = 0")
	}
	if r.NumRegs > 3 {
		t.Errorf("NumRegs = %d, expected <=3 (reuse after last use)", r.NumRegs)
	}
	// d's register must differ from a's reg at point of d's def
	// (a is dead by then so reuse is fine; just sanity-check non-negative)
	if r.Reg[d] < 0 {
		t.Errorf("d has no register")
	}
}

func TestNoUnusedValue(t *testing.T) {
	// param p; dead = p+p; ret p
	b := ir.NewBuilder("f", []ir.Type{ir.TI64}, ir.TI64)
	p := b.Param(0)
	dead := b.AddI64(p, p)
	_ = dead
	b.Ret(p)
	fn := b.Function()
	r := Run(fn, nil)
	// dead is included in the allocation (DCE hasn't run); both p and
	// dead get registers. Just confirm everything alive has a reg.
	for v, reg := range r.Reg {
		if v == int(dead) {
			continue
		}
		if reg < 0 {
			t.Errorf("value %d unassigned", v)
		}
	}
}
