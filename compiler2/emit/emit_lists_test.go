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
