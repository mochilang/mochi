package emit

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/ir"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// TestEmitConcatStr exercises the end-to-end path for the string
// concat opcode: source-level "foo" ++ "bar" should compile, run, and
// produce a six-byte string.
func TestEmitConcatStr(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	a := b.ConstStr("foo")
	c := b.ConstStr("bar")
	cat := b.ConcatStr(a, c)
	n := b.LenStr(cat)
	b.Ret(n)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != 6 {
		t.Fatalf("len(foo++bar) = %d, want 6", got.Int())
	}
}

// TestEmitStringPoolDedup confirms that two ConstStr literals with
// the same bytes share an Aux index (and thus a single StrConsts
// entry in the emitted vm2 function).
func TestEmitStringPoolDedup(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TI64)
	a := b.ConstStr("dup")
	c := b.ConstStr("dup")
	_ = b.EqualStr(a, c)
	b.Ret(b.LenStr(a))
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	if got := len(b.Function().Strings); got != 1 {
		t.Fatalf("ir Strings pool = %d, want 1 (dedup)", got)
	}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	if got := len(p.Funcs[0].StrConsts); got != 1 {
		t.Fatalf("vm2 StrConsts = %d, want 1", got)
	}
}

// TestEmitStringsConcatLoopCorpus runs the corpus benchmark for a
// small N through the full opt+emit+run pipeline, asserting it
// produces the oracle's expected length. This is the same path the
// vm2runner subprocess takes, just in-process.
func TestEmitStringsConcatLoopCorpus(t *testing.T) {
	const n = 32
	mod := corpus.BuildStringsConcatLoop(n)
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
	if want := corpus.ExpectStringsConcatLoop(n); got.Int() != want {
		t.Fatalf("concat_loop(%d) = %d, want %d", n, got.Int(), want)
	}
}
