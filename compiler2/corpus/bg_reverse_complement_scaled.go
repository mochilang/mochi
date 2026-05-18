package corpus

import "mochi/compiler2/ir"

// BuildReverseComplement is the scaled cross-lang companion to
// BuildReverseComplementKernel. The kernel form uses a hard-coded
// N=1024 buffer; this form takes N as a parameter so the cross-lang
// harness can sweep buffer sizes.
//
// The arithmetic boils down to:
//
//	in := bytes[0..N), filled with the repeating "ACGT" pattern
//	out[N-1-i] := complement(in[i]) for every i in [0, N)
//	return sum(out)
//
// When N is a multiple of 4, the sum is deterministic and equals
// (N/4) * (65+67+71+84) = (N/4) * 287; the cross-lang harness uses
// this single i64 to integer-compare every peer.
//
// Six hand-written IR functions, identical in shape to the kernel
// form:
//
//	main()                       = fillInput; rcLoop; sumBytes; return sum
//	fillInput(in, i, n)          = tail-rec, in[i] = baseFor(i%4)
//	baseFor(k) -> i64            = "ACGT"[k] dispatch (k in 0..3)
//	rcLoop(in, out, i, n)        = tail-rec, out[n-1-i] = complement(in[i])
//	complement(c) -> i64         = A<->T, C<->G dispatch on the input byte
//	sumBytes(arr, i, n, acc)     = tail-rec accumulator
//
// All recursive loops are tail-call shaped. The accumulator
// (sumBytes) uses Ret(call_result) so opt.TailCall folds it into a
// single OpTailCallSelfA4; the other helpers return TUnit and use
// Ret(-1) after the recursive call, the same idiom the kernel form
// uses (and the same idiom every other scaled BG builder uses for
// TUnit recursion).
func BuildReverseComplement(n int64) *ir.Module {
	const (
		fillIdx = 1
		baseIdx = 2
		rcIdx   = 3
		compIdx = 4
		sumIdx  = 5
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	in := bMain.NewU8Array(nv)
	out := bMain.NewU8Array(nv)
	bMain.Call(fillIdx, ir.TUnit, in, bMain.ConstI64(0), nv)
	bMain.Call(rcIdx, ir.TUnit, in, out, bMain.ConstI64(0), nv)
	sum := bMain.Call(sumIdx, ir.TI64, out, bMain.ConstI64(0), nv, bMain.ConstI64(0))
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

	// complement(c) -> i64: A(65)<->T(84), C(67)<->G(71). Other bytes pass through.
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

// ExpectReverseComplement runs the same algorithm in plain Go so the
// oracle test can assert vm2 produces the bit-identical int64 result.
func ExpectReverseComplement(n int64) int64 {
	in := make([]byte, n)
	bases := [4]byte{'A', 'C', 'G', 'T'}
	for i := int64(0); i < n; i++ {
		in[i] = bases[i%4]
	}
	out := make([]byte, n)
	for i := int64(0); i < n; i++ {
		var c byte
		switch in[i] {
		case 'A':
			c = 'T'
		case 'T':
			c = 'A'
		case 'C':
			c = 'G'
		case 'G':
			c = 'C'
		default:
			c = in[i]
		}
		out[n-1-i] = c
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += int64(out[i])
	}
	return sum
}
