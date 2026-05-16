package opt

import (
	"testing"

	"mochi/compiler2/emit"
	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// buildSumTo builds:
//
//	sum(n, acc) = if n == 0 then acc else sum(n-1, acc+n)
//	main()     = sum(N, 0)
func buildSumTo(N int64) *ir.Module {
	const sumIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	n := bMain.ConstI64(N)
	z := bMain.ConstI64(0)
	r := bMain.Call(sumIdx, ir.TI64, n, z)
	bMain.Ret(r)

	bSum := ir.NewBuilder("sum", []ir.Type{ir.TI64, ir.TI64}, ir.TI64)
	pN := bSum.Param(0)
	pAcc := bSum.Param(1)
	thenB := bSum.NewBlock()
	elseB := bSum.NewBlock()

	c0 := bSum.ConstI64(0)
	cond := bSum.EqualI64(pN, c0)
	bSum.CondBr(cond, thenB, elseB)

	bSum.SwitchTo(thenB)
	bSum.Ret(pAcc)

	bSum.SwitchTo(elseB)
	c1 := bSum.ConstI64(1)
	nm1 := bSum.SubI64(pN, c1)
	acc1 := bSum.AddI64(pAcc, pN)
	r2 := bSum.Call(sumIdx, ir.TI64, nm1, acc1)
	bSum.Ret(r2) // tail position

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bSum.Function()}, Main: 0}
}

func TestTailCallRewritesSumTo(t *testing.T) {
	m := buildSumTo(10)
	n := 0
	for _, f := range m.Funcs {
		n += TailCall(f)
	}
	// Two rewrites: main's call to sum, and sum.elseB's recursive call.
	if n != 2 {
		t.Fatalf("TailCall rewrote %d sites, want 2", n)
	}
	// The else block of sum should now end in OpTailCall.
	sum := m.Funcs[1]
	for _, blk := range sum.Blocks {
		last := sum.Values[blk.Insts[len(blk.Insts)-1]]
		if last.Op == ir.OpTailCall {
			return
		}
	}
	t.Fatal("no OpTailCall found after rewrite")
}

func TestTailCallSumToRunsCorrectly(t *testing.T) {
	m := buildSumTo(100)
	for _, f := range m.Funcs {
		TailCall(f)
	}
	p, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	want := int64(100 * 101 / 2)
	if got.Int() != want {
		t.Fatalf("sum(100) = %d, want %d", got.Int(), want)
	}
}

func TestTailCallEnablesDeepRecursion(t *testing.T) {
	// 100_000 frames would blow without tail-call.
	m := buildSumTo(100_000)
	for _, f := range m.Funcs {
		TailCall(f)
	}
	p, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	got, err := vm2.New(p).Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	want := int64(100_000) * 100_001 / 2
	if got.Int() != want {
		t.Fatalf("sum(100000) = %d, want %d", got.Int(), want)
	}
}
