package corpus

import "mochi/compiler2/ir"

// BuildReverseComplementKernel builds the reverse_complement BG inner
// kernel (MEP-37 §3.4). The full canonical program reads FASTA from
// stdin, complements each DNA base (A↔T, C↔G, a↔t, c↔g), and writes the
// reversed-complement stream to stdout in 60-char lines. The kernel
// here keeps the arithmetic core (per-byte complement and an in-place
// reverse) on a fixed 1024-byte buffer of generated input, then sums
// the output bytes so the whole program reduces to a single i64 the
// test can compare against a Go reference.
//
// Six hand-written IR functions:
//
//	main()                       = fillInput; rcLoop; sumBytes; return sum
//	fillInput(in, i, n)          = tail-rec, sets in[i] = baseFor(i%4)
//	baseFor(k) -> i64            = "ACGT"[k] dispatch (k in 0..3)
//	rcLoop(in, out, i, n)        = tail-rec, sets out[n-1-i] = complement(in[i])
//	complement(c) -> i64         = A<->T, C<->G dispatch on the input byte
//	sumBytes(arr, i, n, acc)     = tail-rec accumulator
//
// All loops tail-recurse and fold through opt.TailCall →
// OpTailCallSelf. The two U8Array allocations (input + output) are the
// only heap touches; everything else stays in registers.
func BuildReverseComplementKernel() *ir.Module {
	const (
		fillIdx = 1
		baseIdx = 2
		rcIdx   = 3
		compIdx = 4
		sumIdx  = 5
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	n := bMain.ConstI64(1024)
	in := bMain.NewU8Array(n)
	out := bMain.NewU8Array(n)
	bMain.Call(fillIdx, ir.TUnit, in, bMain.ConstI64(0), n)
	bMain.Call(rcIdx, ir.TUnit, in, out, bMain.ConstI64(0), n)
	sum := bMain.Call(sumIdx, ir.TI64, out, bMain.ConstI64(0), n, bMain.ConstI64(0))
	bMain.Ret(sum)

	// fillInput(in, i, n): in[i] = baseFor(i%4); recurse with i+1 until i>=n.
	bF := ir.NewBuilder("fillInput",
		[]ir.Type{ir.TU8Array, ir.TI64, ir.TI64}, ir.TUnit)
	fIn, fI, fN := bF.Param(0), bF.Param(1), bF.Param(2)
	fDone := bF.NewBlock()
	fStep := bF.NewBlock()
	bF.CondBr(bF.LessI64(fI, fN), fStep, fDone)
	bF.SwitchTo(fDone)
	bF.Ret(-1)
	bF.SwitchTo(fStep)
	mod4 := bF.ModI64(fI, bF.ConstI64(4))
	v := bF.Call(baseIdx, ir.TI64, mod4)
	bF.U8ArrSet(fIn, fI, v)
	bF.Call(fillIdx, ir.TUnit, fIn,
		bF.AddI64(fI, bF.ConstI64(1)), fN)
	bF.Ret(-1)

	// baseFor(k) -> i64: 0='A'(65), 1='C'(67), 2='G'(71), 3='T'(84).
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

	// rcLoop(in, out, i, n): out[n-1-i] = complement(in[i]); recurse with i+1.
	bR := ir.NewBuilder("rcLoop",
		[]ir.Type{ir.TU8Array, ir.TU8Array, ir.TI64, ir.TI64}, ir.TUnit)
	rIn, rOut, rI, rN := bR.Param(0), bR.Param(1), bR.Param(2), bR.Param(3)
	rDone := bR.NewBlock()
	rStep := bR.NewBlock()
	bR.CondBr(bR.LessI64(rI, rN), rStep, rDone)
	bR.SwitchTo(rDone)
	bR.Ret(-1)
	bR.SwitchTo(rStep)
	srcByte := bR.U8ArrGet(rIn, rI)
	comp := bR.Call(compIdx, ir.TI64, srcByte)
	dstIdx := bR.SubI64(bR.SubI64(rN, bR.ConstI64(1)), rI)
	bR.U8ArrSet(rOut, dstIdx, comp)
	bR.Call(rcIdx, ir.TUnit, rIn, rOut,
		bR.AddI64(rI, bR.ConstI64(1)), rN)
	bR.Ret(-1)

	// complement(c) -> i64: A(65)↔T(84), C(67)↔G(71). Other bytes pass through.
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

	// sumBytes(arr, i, n, acc): tail-rec accumulator.
	bS := ir.NewBuilder("sumBytes",
		[]ir.Type{ir.TU8Array, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sArr, sI, sN, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sI, sN), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	cell := bS.U8ArrGet(sArr, sI)
	rec := bS.Call(sumIdx, ir.TI64, sArr,
		bS.AddI64(sI, bS.ConstI64(1)), sN,
		bS.AddI64(sAcc, cell))
	bS.Ret(rec)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bF.Function(), bB.Function(),
		bR.Function(), bC.Function(), bS.Function(),
	}, Main: 0}
}
