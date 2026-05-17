package corpus

import "mochi/compiler2/ir"

// BuildFannkuchRedux is the cross-language parameterised companion to
// BuildFannkuchReduxKernel. The kernel form fixes N=7 and a single
// starting permutation; this form runs the count_flips inner loop n
// times across n rotated 7-element permutations and returns the sum of
// per-trial flip counts. The shape matches the .py/.lua/.go peers in
// bench/template/bg/fannkuch_redux/ so cross-lang rows are output-
// equivalent.
//
// Each trial constructs perm[i] = ((i + k) mod 7) + 1 for k in [0, n).
// count_flips destructively reverses prefixes until perm[0] == 1; the
// trial count is summed into the return value.
//
// Rationale for fixed inner permutation size 7 (rather than parameter
// N for both): vm2 has no allocate-array-at-runtime-of-variable-size
// pattern in the corpus today (NewI64Array takes a Value but the IR
// builders pass a constant). Holding the inner size at 7 lets us reuse
// the same kernel size as the existing BG canonical reference (where
// 7 is the size used in the BG submitted code at small sizes). The
// outer count n controls per-invocation work.
func BuildFannkuchRedux(n int64) *ir.Module {
	const (
		outerIdx = 1
		flipIdx  = 2
		revIdx   = 3
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	permSize := bMain.ConstI64(7)
	perm := bMain.NewI64Array(permSize)
	nv := bMain.ConstI64(n)
	zero := bMain.ConstI64(0)
	total := bMain.Call(outerIdx, ir.TI64, perm, zero, nv, zero)
	bMain.Ret(total)

	bO := ir.NewBuilder("outer",
		[]ir.Type{ir.TI64Array, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	oP, oK, oN, oT := bO.Param(0), bO.Param(1), bO.Param(2), bO.Param(3)
	oDone := bO.NewBlock()
	oStep := bO.NewBlock()
	bO.CondBr(bO.LessI64(oK, oN), oStep, oDone)
	bO.SwitchTo(oDone)
	bO.Ret(oT)
	bO.SwitchTo(oStep)
	// Reset perm to a fresh rotation of [1..7]: perm[i] = ((i + k) % 7) + 1.
	sevenK := bO.ConstI64(7)
	for i := int64(0); i < 7; i++ {
		idx := bO.ConstI64(i)
		sum := bO.AddI64(idx, oK)
		modd := bO.ModI64(sum, sevenK)
		val := bO.AddI64(modd, bO.ConstI64(1))
		bO.I64ArrSet(oP, idx, val)
	}
	cf := bO.Call(flipIdx, ir.TI64, oP, bO.ConstI64(0))
	nextK := bO.AddI64(oK, bO.ConstI64(1))
	nextT := bO.AddI64(oT, cf)
	rec := bO.Call(outerIdx, ir.TI64, oP, nextK, oN, nextT)
	bO.Ret(rec)

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
	rec2 := bF.Call(flipIdx, ir.TI64, fPerm,
		bF.AddI64(fCount, bF.ConstI64(1)))
	bF.Ret(rec2)

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
		bMain.Function(), bO.Function(), bF.Function(), bR.Function(),
	}, Main: 0}
}

// ExpectFannkuchRedux computes the same sum of per-trial flip counts in
// plain Go so tests can assert equivalence without building IR.
func ExpectFannkuchRedux(n int64) int64 {
	perm := [7]int64{}
	total := int64(0)
	for k := int64(0); k < n; k++ {
		for i := int64(0); i < 7; i++ {
			perm[i] = ((i+k)%7 + 1)
		}
		flips := int64(0)
		for perm[0] != 1 {
			lo, hi := int64(0), perm[0]-1
			for lo < hi {
				perm[lo], perm[hi] = perm[hi], perm[lo]
				lo++
				hi--
			}
			flips++
		}
		total += flips
	}
	return total
}
