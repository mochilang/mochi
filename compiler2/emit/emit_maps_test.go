package emit

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/ir"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// TestEmitMapSetGet exercises the end-to-end path for the map
// subsystem: NewMap -> Set -> Get -> Len, compiled and run.
func TestEmitMapSetGet(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	m := b.NewMap()
	k1 := b.ConstI64(1)
	v1 := b.ConstI64(11)
	b.MapSet(m, k1, v1)
	k2 := b.ConstI64(2)
	v2 := b.ConstI64(22)
	b.MapSet(m, k2, v2)
	got := b.MapGet(m, k2, ir.TI64)
	b.Ret(got)
	mod := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(mod)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	out, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if out.Int() != 22 {
		t.Fatalf("m[2] = %d, want 22", out.Int())
	}
}

// TestEmitMapsFillSumCorpus runs the corpus map benchmark through the
// full opt+emit+run pipeline and asserts it matches the oracle.
func TestEmitMapsFillSumCorpus(t *testing.T) {
	const n = 32
	mod := corpus.BuildMapsFillSum(n)
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
	if want := corpus.ExpectMapsFillSum(n); got.Int() != want {
		t.Fatalf("maps_fill_sum(%d) = %d, want %d", n, got.Int(), want)
	}
}
