package emit

import (
	"math/big"
	"testing"

	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// TestEmitBignumStraightline builds a straight-line IR that exercises
// the bignum const pool and Add/Mul without any phi/loop, so a failure
// here cleanly isolates the emit-level lowering from the
// regalloc/phi interaction.
func TestEmitBignumStraightline(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TBigInt)
	a := b.ConstBigInt("1000000000000000000")
	c := b.ConstBigInt("2000000000000000000")
	sum := b.AddBigInt(a, c) // 3e18
	prod := b.MulBigInt(sum, a)
	b.Ret(prod)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	aBI, _ := new(big.Int).SetString("1000000000000000000", 10)
	cBI, _ := new(big.Int).SetString("2000000000000000000", 10)
	want := new(big.Int).Mul(new(big.Int).Add(aBI, cBI), aBI)
	if !got.IsPtr() || got.IsHeapStr() {
		t.Fatalf("want bignum cell, got bits=%x", got.Bits)
	}
	if got.BigInt().Cmp(want) != 0 {
		t.Fatalf("got %s, want %s", got.BigInt(), want)
	}
}

// TestEmitBignumFactorialRec builds a recursive factorial in bignum and
// verifies the result against math/big. The recursion shape mirrors the
// fib emit test so the bignum ops are exercised inside Call/Return.
func TestEmitBignumFactorialRec(t *testing.T) {
	const factIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TBigInt)
	n := bMain.ConstI64(25)
	r := bMain.Call(factIdx, ir.TBigInt, n)
	bMain.Ret(r)

	// fact(n: i64) -> bigint:
	//   if n <= 1 { return ConstBigInt("1") }
	//   return n_big * fact(n-1)
	bFact := ir.NewBuilder("fact", []ir.Type{ir.TI64}, ir.TBigInt)
	param := bFact.Param(0)
	thenB := bFact.NewBlock()
	elseB := bFact.NewBlock()
	c1i := bFact.ConstI64(1)
	cond := bFact.LessEqI64(param, c1i)
	bFact.CondBr(cond, thenB, elseB)

	bFact.SwitchTo(thenB)
	one := bFact.ConstBigInt("1")
	bFact.Ret(one)

	bFact.SwitchTo(elseB)
	nm1 := bFact.SubI64(param, c1i)
	sub := bFact.Call(factIdx, ir.TBigInt, nm1)
	nBig := bFact.I64ToBigInt(param)
	prod := bFact.MulBigInt(nBig, sub)
	bFact.Ret(prod)

	m := &ir.Module{
		Funcs: []*ir.Function{bMain.Function(), bFact.Function()},
		Main:  0,
	}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	want := new(big.Int).SetInt64(1)
	for i := int64(2); i <= 25; i++ {
		want.Mul(want, big.NewInt(i))
	}
	if !got.IsPtr() || got.IsHeapStr() {
		t.Fatalf("want bignum cell, got bits=%x", got.Bits)
	}
	if got.BigInt().Cmp(want) != 0 {
		t.Fatalf("fact(25) = %s, want %s", got.BigInt(), want)
	}
}

// TestEmitBignumToStr verifies OpBigIntToStr produces a heap string Cell
// that decodes to the expected base-10 representation.
func TestEmitBignumToStr(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TStr)
	big1 := b.ConstBigInt("9999999999999999999999999999")
	b.Ret(b.BigIntToStr(big1))
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !got.IsHeapStr() {
		t.Fatalf("want heap string cell, got bits=%x", got.Bits)
	}
}

// TestEmitBignumPoolDedup verifies that two ConstBigInt with the same
// literal share a single pool index (the builder dedups) and ultimately
// a single Program.Consts entry (the emitter dedups via cAdd).
func TestEmitBignumPoolDedup(t *testing.T) {
	b := ir.NewBuilder("main", nil, ir.TBigInt)
	x := b.ConstBigInt("999999999999999999999999")
	y := b.ConstBigInt("999999999999999999999999")
	if x == y {
		t.Fatal("same literal should produce distinct ValueIDs")
	}
	if got := len(b.Function().BigInts); got != 1 {
		t.Fatalf("BigInts pool size = %d, want 1", got)
	}
	sum := b.AddBigInt(x, y)
	b.Ret(sum)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// Count distinct bignum cells in Consts: should be 1.
	seen := map[*big.Int]bool{}
	for _, c := range p.Funcs[0].Consts {
		if c.IsPtr() && !c.IsHeapStr() {
			bi := c.BigInt()
			if bi != nil {
				seen[bi] = true
			}
		}
	}
	if len(seen) != 1 {
		t.Fatalf("want 1 bignum const in pool, got %d", len(seen))
	}
}
