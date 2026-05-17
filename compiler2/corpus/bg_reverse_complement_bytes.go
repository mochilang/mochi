package corpus

import "mochi/compiler2/ir"

// BuildReverseComplementBytesKernel is the MEP-38 §3.1 byte-view
// variant of BuildReverseComplementKernel. The arithmetic is identical
// (per-byte ACGT complement of a 1024-byte buffer, sum the output)
// but storage is a single owning vmBytes view rather than two
// vmU8Arrays. The kernel exercises:
//
//   - OpBytesNew: owning view allocation
//   - OpBytesSet: writes into the owning view
//   - OpBytesGet: reads from the owning view
//   - OpBytesLen: bound to the loop test (here baked in as a const)
//
// Six hand-written IR functions, same shape as the U8Array variant
// but every U8ArrGet/Set replaced by BytesGet/Set, and the second
// buffer eliminated (in-place reverse-and-complement is not used; the
// kernel uses out[n-1-i] = complement(in[i]) so both indices land in
// the same view but never collide).
//
// The two output cells start at i=0 (writing to out[n-1-0] = out[1023])
// and the input cells start at the same indices that fill produced.
// Since fillInput pre-populates the WHOLE view with ACGT, and rcLoop
// reads in[i] then writes out[n-1-i], the read-before-write contract
// is preserved as long as i never equals n-1-i, i.e. n is even (1024 is).
// At i == n/2 the read and write target adjacent bytes (i=511, n-1-i=512)
// so no aliasing hazard occurs. This is the same trick reverse_complement's
// canonical form uses: a single buffer, in-place reverse-and-complement.
func BuildReverseComplementBytesKernel() *ir.Module {
	const (
		fillIdx = 1
		baseIdx = 2
		rcIdx   = 3
		compIdx = 4
		sumIdx  = 5
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	n := bMain.ConstI64(1024)
	buf := bMain.NewBytes(n)
	bMain.Call(fillIdx, ir.TUnit, buf, bMain.ConstI64(0), n)
	bMain.Call(rcIdx, ir.TUnit, buf, bMain.ConstI64(0), n)
	sum := bMain.Call(sumIdx, ir.TI64, buf, bMain.ConstI64(0), n, bMain.ConstI64(0))
	bMain.Ret(sum)

	// fillInput(buf, i, n): buf[i] = baseFor(i%4); recurse with i+1.
	bF := ir.NewBuilder("fillInput",
		[]ir.Type{ir.TBytes, ir.TI64, ir.TI64}, ir.TUnit)
	fBuf, fI, fN := bF.Param(0), bF.Param(1), bF.Param(2)
	fDone := bF.NewBlock()
	fStep := bF.NewBlock()
	bF.CondBr(bF.LessI64(fI, fN), fStep, fDone)
	bF.SwitchTo(fDone)
	bF.Ret(-1)
	bF.SwitchTo(fStep)
	mod4 := bF.ModI64(fI, bF.ConstI64(4))
	v := bF.Call(baseIdx, ir.TI64, mod4)
	bF.BytesSet(fBuf, fI, v)
	bF.Call(fillIdx, ir.TUnit, fBuf,
		bF.AddI64(fI, bF.ConstI64(1)), fN)
	bF.Ret(-1)

	// baseFor(k): 0='A', 1='C', 2='G', 3='T'.
	bB := ir.NewBuilder("baseFor", []ir.Type{ir.TI64}, ir.TI64)
	bK := bB.Param(0)
	bbA := bB.NewBlock()
	bbChk1 := bB.NewBlock()
	bbC := bB.NewBlock()
	bbChk2 := bB.NewBlock()
	bbG := bB.NewBlock()
	bbT := bB.NewBlock()
	bB.CondBr(bB.EqualI64(bK, bB.ConstI64(0)), bbA, bbChk1)
	bB.SwitchTo(bbA)
	bB.Ret(bB.ConstI64(65))
	bB.SwitchTo(bbChk1)
	bB.CondBr(bB.EqualI64(bK, bB.ConstI64(1)), bbC, bbChk2)
	bB.SwitchTo(bbC)
	bB.Ret(bB.ConstI64(67))
	bB.SwitchTo(bbChk2)
	bB.CondBr(bB.EqualI64(bK, bB.ConstI64(2)), bbG, bbT)
	bB.SwitchTo(bbG)
	bB.Ret(bB.ConstI64(71))
	bB.SwitchTo(bbT)
	bB.Ret(bB.ConstI64(84))

	// rcLoop(buf, i, n): only walk i in [0, n/2) so the read at buf[i]
	// happens before the write at buf[n-1-i]; at each step we also write
	// the complementary side. This is the canonical in-place reverse-
	// and-complement loop.
	bR := ir.NewBuilder("rcLoop",
		[]ir.Type{ir.TBytes, ir.TI64, ir.TI64}, ir.TUnit)
	rBuf, rI, rN := bR.Param(0), bR.Param(1), bR.Param(2)
	rDone := bR.NewBlock()
	rStep := bR.NewBlock()
	half := bR.DivI64(rN, bR.ConstI64(2))
	bR.CondBr(bR.LessI64(rI, half), rStep, rDone)
	bR.SwitchTo(rDone)
	bR.Ret(-1)
	bR.SwitchTo(rStep)
	j := bR.SubI64(bR.SubI64(rN, bR.ConstI64(1)), rI)
	leftByte := bR.BytesGet(rBuf, rI)
	rightByte := bR.BytesGet(rBuf, j)
	leftComp := bR.Call(compIdx, ir.TI64, leftByte)
	rightComp := bR.Call(compIdx, ir.TI64, rightByte)
	bR.BytesSet(rBuf, j, leftComp)
	bR.BytesSet(rBuf, rI, rightComp)
	bR.Call(rcIdx, ir.TUnit, rBuf,
		bR.AddI64(rI, bR.ConstI64(1)), rN)
	bR.Ret(-1)

	// complement(c): A(65)<->T(84), C(67)<->G(71).
	bC := ir.NewBuilder("complement", []ir.Type{ir.TI64}, ir.TI64)
	cC := bC.Param(0)
	cIsA := bC.NewBlock()
	cChkT := bC.NewBlock()
	cIsT := bC.NewBlock()
	cChkC := bC.NewBlock()
	cIsC := bC.NewBlock()
	cChkG := bC.NewBlock()
	cIsG := bC.NewBlock()
	cElse := bC.NewBlock()
	bC.CondBr(bC.EqualI64(cC, bC.ConstI64(65)), cIsA, cChkT)
	bC.SwitchTo(cIsA)
	bC.Ret(bC.ConstI64(84))
	bC.SwitchTo(cChkT)
	bC.CondBr(bC.EqualI64(cC, bC.ConstI64(84)), cIsT, cChkC)
	bC.SwitchTo(cIsT)
	bC.Ret(bC.ConstI64(65))
	bC.SwitchTo(cChkC)
	bC.CondBr(bC.EqualI64(cC, bC.ConstI64(67)), cIsC, cChkG)
	bC.SwitchTo(cIsC)
	bC.Ret(bC.ConstI64(71))
	bC.SwitchTo(cChkG)
	bC.CondBr(bC.EqualI64(cC, bC.ConstI64(71)), cIsG, cElse)
	bC.SwitchTo(cIsG)
	bC.Ret(bC.ConstI64(67))
	bC.SwitchTo(cElse)
	bC.Ret(cC)

	// sumBytes(buf, i, n, acc): tail-rec accumulator over a TBytes view.
	bS := ir.NewBuilder("sumBytes",
		[]ir.Type{ir.TBytes, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sBuf, sI, sN, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sI, sN), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	cell := bS.BytesGet(sBuf, sI)
	rec := bS.Call(sumIdx, ir.TI64, sBuf,
		bS.AddI64(sI, bS.ConstI64(1)), sN,
		bS.AddI64(sAcc, cell))
	bS.Ret(rec)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bF.Function(), bB.Function(),
		bR.Function(), bC.Function(), bS.Function(),
	}, Main: 0}
}
