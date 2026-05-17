package corpus

import "mochi/compiler2/ir"

// BuildMandelbrotKernel builds the mandelbrot BG kernel as described in
// MEP-37 §3.2 (FMA in the iteration) / §3.3 (U8 bitmap output).
//
// For each pixel (row, col) of a w x h grid, the kernel maps it to the
// complex plane (cx, cy) in [-2, 1] x [-1, 1] and iterates
//
//	z_{n+1} = z_n^2 + c
//
// until |z|^2 > 4 or n reaches maxIter. The escape count clamped to
// maxIter is stored as a byte into a U8Array of length w*h. main()
// returns the sum of all escape counts so a Go peer can verify the
// kernel bit-for-bit.
//
// The inner iteration is FMA-shaped on the imaginary axis: the spec
// calls out zi_{n+1} = 2*zr*zi + cy as a one-rounding FMA, and the
// builder emits OpFmaF64 directly for that step. The real axis stays as
// the naive (zr*zr - zi*zi) + cx form because that pattern is a SUB+ADD
// rather than a MUL+ADD; FMA-folding it would need an FMS opcode we
// have not added yet.
//
// All loops are written as tail-recursive helpers so opt.TailCall folds
// each into OpTailCallSelf. The recursive shape (Call; Ret r) is
// preserved for any helper whose return value participates in the
// caller's result; the same constraint we hit in n_body's energy
// accumulator.
func BuildMandelbrotKernel(w, h, maxIter int64) *ir.Module {
	const (
		rowIdx  = 1
		colIdx  = 2
		iterIdx = 3
		sumIdx  = 4
	)

	// main: out = newU8Array(w*h); rowLoop(out, 0, ...); return sum(out).
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	wv := bMain.ConstI64(w)
	hv := bMain.ConstI64(h)
	miv := bMain.ConstI64(maxIter)
	n := bMain.MulI64(wv, hv)
	out := bMain.NewU8Array(n)
	zero := bMain.ConstI64(0)
	bMain.Call(rowIdx, ir.TUnit, out, zero, wv, hv, miv)
	sum := bMain.Call(sumIdx, ir.TI64, out, zero, n, zero)
	bMain.Ret(sum)

	// rowLoop(out, row, w, h, mi): if row >= h return; colLoop; recurse row+1.
	bR := ir.NewBuilder("rowLoop",
		[]ir.Type{ir.TU8Array, ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TUnit)
	rOut, rRow, rW, rH, rMi := bR.Param(0), bR.Param(1), bR.Param(2), bR.Param(3), bR.Param(4)
	rDone := bR.NewBlock()
	rStep := bR.NewBlock()
	bR.CondBr(bR.LessI64(rRow, rH), rStep, rDone)
	bR.SwitchTo(rDone)
	bR.Ret(-1)
	bR.SwitchTo(rStep)
	bR.Call(colIdx, ir.TUnit, rOut, rRow, bR.ConstI64(0), rW, rH, rMi)
	bR.Call(rowIdx, ir.TUnit, rOut,
		bR.AddI64(rRow, bR.ConstI64(1)), rW, rH, rMi)
	bR.Ret(-1)

	// colLoop(out, row, col, w, h, mi):
	//   if col >= w return
	//   cx = col/w * 3.0 - 2.0
	//   cy = row/h * 2.0 - 1.0
	//   n  = iterate(0, 0, cx, cy, 0, mi)
	//   out[row*w + col] = n
	//   recurse col+1
	bC := ir.NewBuilder("colLoop",
		[]ir.Type{ir.TU8Array, ir.TI64, ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TUnit)
	cOut, cRow, cCol, cW, cH, cMi := bC.Param(0), bC.Param(1), bC.Param(2), bC.Param(3), bC.Param(4), bC.Param(5)
	cDone := bC.NewBlock()
	cStep := bC.NewBlock()
	bC.CondBr(bC.LessI64(cCol, cW), cStep, cDone)
	bC.SwitchTo(cDone)
	bC.Ret(-1)
	bC.SwitchTo(cStep)
	colF := bC.I64ToF64(cCol)
	wF := bC.I64ToF64(cW)
	cx := bC.SubF64(bC.MulF64(bC.DivF64(colF, wF), bC.ConstF64(3)),
		bC.ConstF64(2))
	rowF := bC.I64ToF64(cRow)
	hF := bC.I64ToF64(cH)
	cy := bC.SubF64(bC.MulF64(bC.DivF64(rowF, hF), bC.ConstF64(2)),
		bC.ConstF64(1))
	iters := bC.Call(iterIdx, ir.TI64,
		bC.ConstF64(0), bC.ConstF64(0), cx, cy, bC.ConstI64(0), cMi)
	idx := bC.AddI64(bC.MulI64(cRow, cW), cCol)
	bC.U8ArrSet(cOut, idx, iters)
	bC.Call(colIdx, ir.TUnit, cOut, cRow,
		bC.AddI64(cCol, bC.ConstI64(1)), cW, cH, cMi)
	bC.Ret(-1)

	// iterate(zr, zi, cx, cy, i, mi):
	//   if i >= mi return mi
	//   r2 = zr*zr; i2 = zi*zi
	//   if r2 + i2 > 4 return i
	//   nzi = FMA(2*zr, zi, cy)
	//   nzr = (r2 - i2) + cx
	//   tail-call iterate(nzr, nzi, cx, cy, i+1, mi)
	bI := ir.NewBuilder("iterate",
		[]ir.Type{ir.TF64, ir.TF64, ir.TF64, ir.TF64, ir.TI64, ir.TI64}, ir.TI64)
	iZr, iZi, iCx, iCy, iI, iMi := bI.Param(0), bI.Param(1), bI.Param(2), bI.Param(3), bI.Param(4), bI.Param(5)
	iMax := bI.NewBlock()
	iCont := bI.NewBlock()
	bI.CondBr(bI.LessI64(iI, iMi), iCont, iMax)
	bI.SwitchTo(iMax)
	bI.Ret(iMi)
	bI.SwitchTo(iCont)
	r2 := bI.MulF64(iZr, iZr)
	i2 := bI.MulF64(iZi, iZi)
	mag := bI.AddF64(r2, i2)
	iEsc := bI.NewBlock()
	iLoop := bI.NewBlock()
	bI.CondBr(bI.LessEqF64(mag, bI.ConstF64(4)), iLoop, iEsc)
	bI.SwitchTo(iEsc)
	bI.Ret(iI)
	bI.SwitchTo(iLoop)
	twoZr := bI.MulF64(bI.ConstF64(2), iZr)
	nzi := bI.FmaF64(twoZr, iZi, iCy)
	nzr := bI.AddF64(bI.SubF64(r2, i2), iCx)
	rec := bI.Call(iterIdx, ir.TI64,
		nzr, nzi, iCx, iCy, bI.AddI64(iI, bI.ConstI64(1)), iMi)
	bI.Ret(rec)

	// sumU8(out, i, n, acc): if i >= n return acc; acc += out[i]; recurse.
	bS := ir.NewBuilder("sumU8",
		[]ir.Type{ir.TU8Array, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sOut, sI, sN, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sI, sN), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	v := bS.U8ArrGet(sOut, sI)
	nAcc := bS.AddI64(sAcc, v)
	sr := bS.Call(sumIdx, ir.TI64, sOut,
		bS.AddI64(sI, bS.ConstI64(1)), sN, nAcc)
	bS.Ret(sr)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bR.Function(), bC.Function(),
		bI.Function(), bS.Function(),
	}, Main: 0}
}
