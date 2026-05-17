package corpus

import "mochi/compiler2/ir"

// BuildNsieve builds the Sieve of Eratosthenes counting primes <= N. The
// program shape mirrors the Computer Language Benchmarks Game `nsieve`
// kernel: allocate a length-(N+1) boolean array, walk i from 2..N, and
// for each unmarked i increment the count and mark every multiple of i
// starting at i*i. Encoded as four functions so each loop folds into a
// self-tail-call after opt.TailCall:
//
//	main()           = fill xs with zeros; outer(xs, 2, N, 0)
//	fill(xs, i, lim) = push 0; recurse(xs, i+1, lim)
//	mark(xs, j, n, step) = xs[j] = 1; recurse(xs, j+step, n, step)
//	outer(xs, i, n, count) = if xs[i]==0 then mark + recurse(xs, i+1, n, count+1)
//	                         else recurse(xs, i+1, n, count)
//
// vm2 has no bool list specialization, so the "marked" flag is stored
// as i64 (0 = unmarked, 1 = composite). This costs ~8x the bytes of a
// dense bool array, which is the point: it is the BG list-payload
// throughput stress.
func BuildNsieve(n int64) *ir.Module {
	const (
		fillIdx  = 1
		markIdx  = 2
		outerIdx = 3
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	xs := bMain.NewList(n + 1)
	zero := bMain.ConstI64(0)
	np1 := bMain.ConstI64(n + 1)
	bMain.Call(fillIdx, ir.TUnit, xs, zero, np1)
	two := bMain.ConstI64(2)
	nv := bMain.ConstI64(n)
	cnt0 := bMain.ConstI64(0)
	r := bMain.Call(outerIdx, ir.TI64, xs, two, nv, cnt0)
	bMain.Ret(r)

	bF := ir.NewBuilder("fill", []ir.Type{ir.TList, ir.TI64, ir.TI64}, ir.TUnit)
	pXs, pI, pLim := bF.Param(0), bF.Param(1), bF.Param(2)
	fDone := bF.NewBlock()
	fStep := bF.NewBlock()
	bF.CondBr(bF.LessI64(pI, pLim), fStep, fDone)
	bF.SwitchTo(fDone)
	bF.Ret(-1)
	bF.SwitchTo(fStep)
	z := bF.ConstI64(0)
	bF.ListPush(pXs, z)
	one := bF.ConstI64(1)
	ni := bF.AddI64(pI, one)
	bF.Call(fillIdx, ir.TUnit, pXs, ni, pLim)
	bF.Ret(-1)

	bM := ir.NewBuilder("mark", []ir.Type{ir.TList, ir.TI64, ir.TI64, ir.TI64}, ir.TUnit)
	mXs, mJ, mN, mS := bM.Param(0), bM.Param(1), bM.Param(2), bM.Param(3)
	mDone := bM.NewBlock()
	mStep := bM.NewBlock()
	bM.CondBr(bM.LessEqI64(mJ, mN), mStep, mDone)
	bM.SwitchTo(mDone)
	bM.Ret(-1)
	bM.SwitchTo(mStep)
	mOne := bM.ConstI64(1)
	bM.ListSet(mXs, mJ, mOne)
	nj := bM.AddI64(mJ, mS)
	bM.Call(markIdx, ir.TUnit, mXs, nj, mN, mS)
	bM.Ret(-1)

	bO := ir.NewBuilder("outer", []ir.Type{ir.TList, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	oXs, oI, oN, oC := bO.Param(0), bO.Param(1), bO.Param(2), bO.Param(3)
	oDone := bO.NewBlock()
	oStep := bO.NewBlock()
	bO.CondBr(bO.LessEqI64(oI, oN), oStep, oDone)
	bO.SwitchTo(oDone)
	bO.Ret(oC)
	bO.SwitchTo(oStep)
	v := bO.ListGet(oXs, oI, ir.TI64)
	zero2 := bO.ConstI64(0)
	isUnmarked := bO.EqualI64(v, zero2)
	primeBranch := bO.NewBlock()
	skipBranch := bO.NewBlock()
	bO.CondBr(isUnmarked, primeBranch, skipBranch)

	bO.SwitchTo(primeBranch)
	jStart := bO.MulI64(oI, oI)
	bO.Call(markIdx, ir.TUnit, oXs, jStart, oN, oI)
	one2 := bO.ConstI64(1)
	nc := bO.AddI64(oC, one2)
	ni2 := bO.AddI64(oI, one2)
	r2 := bO.Call(outerIdx, ir.TI64, oXs, ni2, oN, nc)
	bO.Ret(r2)

	bO.SwitchTo(skipBranch)
	one3 := bO.ConstI64(1)
	ni3 := bO.AddI64(oI, one3)
	r3 := bO.Call(outerIdx, ir.TI64, oXs, ni3, oN, oC)
	bO.Ret(r3)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bF.Function(), bM.Function(), bO.Function(),
	}, Main: 0}
}

// ExpectNsieve returns the number of primes p where 2 <= p <= n.
func ExpectNsieve(n int64) int64 {
	if n < 2 {
		return 0
	}
	xs := make([]bool, n+1)
	count := int64(0)
	for i := int64(2); i <= n; i++ {
		if !xs[i] {
			count++
			for j := i * i; j <= n; j += i {
				xs[j] = true
			}
		}
	}
	return count
}
