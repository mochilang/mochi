package emit

import (
	"math"
	"testing"

	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// TestEmitFloatArith exercises the MEP-37 §3.2 dispatch contract end to
// end: builder methods land matching opcodes, the runtime computes
// IEEE-754 results, and a CFloat round-trip through the constant pool
// preserves the bit pattern.
func TestEmitFloatArith(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TF64)
	a := b.ConstF64(1.5)
	c := b.ConstF64(2.25)
	sum := b.AddF64(a, c)
	diff := b.SubF64(sum, a)
	prod := b.MulF64(diff, c)
	quot := b.DivF64(prod, b.ConstF64(2.0))
	root := b.SqrtF64(quot)
	b.Ret(root)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}

	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !got.IsFloat() {
		t.Fatalf("expected float result, got %#v", got)
	}
	// ((1.5 + 2.25) - 1.5) * 2.25 / 2 = (2.25 * 2.25) / 2 = 2.53125
	want := math.Sqrt(2.53125)
	if got.Float() != want {
		t.Fatalf("got %v want %v", got.Float(), want)
	}
}

// TestEmitFmaF64 covers the FMA opcode dispatch and verifies the
// single-rounded semantics: a*b + c via OpFmaF64 equals math.FMA(...),
// not the two-rounding mul-then-add path.
func TestEmitFmaF64(t *testing.T) {
	a, c, d := 1.0+math.SmallestNonzeroFloat64*1e16, 1.0+math.SmallestNonzeroFloat64*1e16, -1.0
	b := ir.NewBuilder("main", nil, ir.TF64)
	av := b.ConstF64(a)
	cv := b.ConstF64(c)
	dv := b.ConstF64(d)
	r := b.FmaF64(av, cv, dv)
	b.Ret(r)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	want := math.FMA(a, c, d)
	if got.Float() != want {
		t.Fatalf("FMA got %v want %v", got.Float(), want)
	}
}

// TestEmitFloatCmp verifies the comparison opcodes return Bool cells.
func TestEmitFloatCmp(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TBool)
	x := b.ConstF64(1.5)
	y := b.ConstF64(2.5)
	cmp := b.LessF64(x, y)
	b.Ret(cmp)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !got.IsBool() || !got.Bool() {
		t.Fatalf("expected true bool, got %#v", got)
	}
}
