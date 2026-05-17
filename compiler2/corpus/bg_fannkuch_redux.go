package corpus

import "mochi/compiler2/ir"

// BuildFannkuchReduxKernel builds the fannkuch_redux BG inner kernel as
// described in MEP-37 §3.3 (boxed []Cell vs flat int array). It counts
// the number of "flips" needed to bring 1 to the head of a 7-element
// permutation, where a flip reverses the prefix whose length is the
// current head value. Mirrors the inner loop of the BG canonical
// fannkuch_redux except the outer permutation generator is replaced by
// a fixed starting permutation [4,2,1,5,7,3,6] so the kernel has a
// single deterministic return value (9 flips) for cross-language
// verification.
//
// Three hand-written IR functions:
//
//	main()                = initialise perm[0..7) = {4,2,1,5,7,3,6}; return countFlips(perm, 0)
//	countFlips(perm, c)   = if perm[0]==1 return c; reverse(perm, 0, perm[0]-1); recurse with c+1
//	reverse(perm, lo, hi) = if lo>=hi return; swap perm[lo] and perm[hi]; recurse (lo+1, hi-1)
//
// countFlips is the only helper whose recursive call participates in
// the caller's result, so it uses the canonical `r = Call; Ret r`
// shape; reverse returns TUnit and uses Ret(-1).
//
// All loops tail-recurse and fold through opt.TailCall →
// OpTailCallSelf. Storage is a single TI64Array of length 7 reused
// across the whole call chain.
func BuildFannkuchReduxKernel() *ir.Module {
	const (
		flipIdx = 1
		revIdx  = 2
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	n := bMain.ConstI64(7)
	perm := bMain.NewI64Array(n)
	for i, v := range []int64{4, 2, 1, 5, 7, 3, 6} {
		bMain.I64ArrSet(perm, bMain.ConstI64(int64(i)), bMain.ConstI64(v))
	}
	count := bMain.Call(flipIdx, ir.TI64, perm, bMain.ConstI64(0))
	bMain.Ret(count)

	// countFlips(perm, count): if perm[0]==1 return count;
	//   reverse(perm, 0, perm[0]-1); recurse with count+1.
	bF := ir.NewBuilder("countFlips",
		[]ir.Type{ir.TI64Array, ir.TI64}, ir.TI64)
	fPerm, fCount := bF.Param(0), bF.Param(1)
	fHead := bF.I64ArrGet(fPerm, bF.ConstI64(0))
	fDone := bF.NewBlock()
	fLoop := bF.NewBlock()
	bF.CondBr(bF.EqualI64(fHead, bF.ConstI64(1)), fDone, fLoop)
	bF.SwitchTo(fDone)
	bF.Ret(fCount)
	bF.SwitchTo(fLoop)
	bF.Call(revIdx, ir.TUnit, fPerm, bF.ConstI64(0),
		bF.SubI64(fHead, bF.ConstI64(1)))
	rec := bF.Call(flipIdx, ir.TI64, fPerm,
		bF.AddI64(fCount, bF.ConstI64(1)))
	bF.Ret(rec)

	// reverse(perm, lo, hi): swap perm[lo] and perm[hi], recurse with (lo+1, hi-1).
	bR := ir.NewBuilder("reverse",
		[]ir.Type{ir.TI64Array, ir.TI64, ir.TI64}, ir.TUnit)
	rPerm, rLo, rHi := bR.Param(0), bR.Param(1), bR.Param(2)
	rDone := bR.NewBlock()
	rStep := bR.NewBlock()
	bR.CondBr(bR.LessI64(rLo, rHi), rStep, rDone)
	bR.SwitchTo(rDone)
	bR.Ret(-1)
	bR.SwitchTo(rStep)
	a := bR.I64ArrGet(rPerm, rLo)
	b := bR.I64ArrGet(rPerm, rHi)
	bR.I64ArrSet(rPerm, rLo, b)
	bR.I64ArrSet(rPerm, rHi, a)
	bR.Call(revIdx, ir.TUnit, rPerm,
		bR.AddI64(rLo, bR.ConstI64(1)),
		bR.SubI64(rHi, bR.ConstI64(1)))
	bR.Ret(-1)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bF.Function(), bR.Function(),
	}, Main: 0}
}
