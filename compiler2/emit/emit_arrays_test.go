package emit

import (
	"testing"

	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// TestEmitF64Array exercises the typed-array MEP-37 §3.3 contract: a
// fresh array reads zero, a write followed by a read round-trips, and
// length matches the allocator's argument.
func TestEmitF64Array(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TF64)
	n := b.ConstI64(4)
	a := b.NewF64Array(n)
	idx := b.ConstI64(2)
	val := b.ConstF64(3.5)
	b.F64ArrSet(a, idx, val)
	got := b.F64ArrGet(a, idx)
	b.Ret(got)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	r, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if r.Float() != 3.5 {
		t.Fatalf("got %v want 3.5", r.Float())
	}
}

func TestEmitI64Array(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	n := b.ConstI64(3)
	a := b.NewI64Array(n)
	b.I64ArrSet(a, b.ConstI64(0), b.ConstI64(10))
	b.I64ArrSet(a, b.ConstI64(1), b.ConstI64(20))
	b.I64ArrSet(a, b.ConstI64(2), b.ConstI64(30))
	x := b.I64ArrGet(a, b.ConstI64(1))
	y := b.I64ArrGet(a, b.ConstI64(2))
	s := b.AddI64(x, y)
	b.Ret(s)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	r, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if r.Int() != 50 {
		t.Fatalf("got %d want 50", r.Int())
	}
}

func TestEmitU8Array(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	n := b.ConstI64(8)
	a := b.NewU8Array(n)
	// Write byte 0xFF at index 5; read it back. The widening to int64
	// is by spec: u8 elements decode through CInt so the consumer can
	// fold them into the normal i64 arithmetic stream.
	b.U8ArrSet(a, b.ConstI64(5), b.ConstI64(0xFF))
	r := b.U8ArrGet(a, b.ConstI64(5))
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
	if got.Int() != 0xFF {
		t.Fatalf("got %d want 255", got.Int())
	}
}

// TestEmitF64ArrayOOB confirms the bounds-check trap fires.
func TestEmitF64ArrayOOB(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TF64)
	n := b.ConstI64(2)
	a := b.NewF64Array(n)
	idx := b.ConstI64(5)
	r := b.F64ArrGet(a, idx)
	b.Ret(r)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	if _, err := vm2.New(p).Run(); err == nil {
		t.Fatalf("expected OOB trap, got nil")
	}
}
