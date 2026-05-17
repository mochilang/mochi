package emit

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/ir"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// TestEmitListPushGet exercises the end-to-end path for the list
// subsystem: NewList -> Push -> Get -> Len, compiled and run.
func TestEmitListPushGet(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	xs := b.NewList(4)
	v := b.ConstI64(7)
	b.ListPush(xs, v)
	v2 := b.ConstI64(11)
	b.ListPush(xs, v2)
	idx := b.ConstI64(1)
	got := b.ListGet(xs, idx, ir.TI64)
	b.Ret(got)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	out, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if out.Int() != 11 {
		t.Fatalf("xs[1] = %d, want 11", out.Int())
	}
}

// TestEmitListAppendInPlace builds a fluent chain of three functional
// appends on a list that is dead after each step. Emit must mark every
// OpListAppend's source operand with InstrFlagBLastUse; the dispatcher
// then mutates the source in place, so the three appends share one
// backing *vmList. This is the MEP-36 Phase 3c motivating workload.
func TestEmitListAppendInPlace(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	xs0 := b.NewList(0)
	xs1 := b.ListAppend(xs0, b.ConstI64(10))
	xs2 := b.ListAppend(xs1, b.ConstI64(20))
	xs3 := b.ListAppend(xs2, b.ConstI64(30))
	got := b.ListGet(xs3, b.ConstI64(2), ir.TI64)
	b.Ret(got)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// Spot-check the encoding: every OpListAppend must carry the
	// last-use bit on operand B, since each xs<i> is read by exactly
	// one downstream instruction.
	saw := 0
	for _, in := range p.Funcs[0].Code {
		if in.Op == vm2.OpListAppend {
			saw++
			if in.Flags&vm2.InstrFlagBLastUse == 0 {
				t.Fatalf("OpListAppend at pc with src reg=%d missing InstrFlagBLastUse", in.B)
			}
		}
	}
	if saw != 3 {
		t.Fatalf("expected 3 OpListAppend dispatches, got %d", saw)
	}
	out, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if out.Int() != 30 {
		t.Fatalf("xs3[2] = %d, want 30", out.Int())
	}
}

// TestEmitListAppendPreservesSource verifies the slow-path semantics:
// when the source list is consumed by a later op besides the append,
// emit must NOT set the last-use bit, so the dispatcher allocates a
// fresh backing array and leaves the source untouched.
func TestEmitListAppendPreservesSource(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	xs0 := b.NewList(2)
	// Seed xs0 with a known element so we can read it back after the
	// append and confirm the source survived.
	b.ListPush(xs0, b.ConstI64(7))
	// xs0 still has one downstream read (the ListLen below), so emit
	// must keep the copy.
	_ = b.ListAppend(xs0, b.ConstI64(99))
	srcLen := b.ListLen(xs0)
	b.Ret(srcLen)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	for _, in := range p.Funcs[0].Code {
		if in.Op == vm2.OpListAppend && in.Flags&vm2.InstrFlagBLastUse != 0 {
			t.Fatalf("OpListAppend should NOT carry InstrFlagBLastUse: source has later reader")
		}
	}
	out, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if out.Int() != 1 {
		t.Fatalf("source list len = %d, want 1 (slow path must preserve source)", out.Int())
	}
}

// TestEmitListsFillSumCorpus runs the corpus list benchmark through
// the full opt+emit+run pipeline and asserts it matches the oracle.
func TestEmitListsFillSumCorpus(t *testing.T) {
	const n = 32
	mod := corpus.BuildListsFillSum(n)
	for _, f := range mod.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	p, err := Compile(mod)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if want := corpus.ExpectListsFillSum(n); got.Int() != want {
		t.Fatalf("fill_sum(%d) = %d, want %d", n, got.Int(), want)
	}
}
