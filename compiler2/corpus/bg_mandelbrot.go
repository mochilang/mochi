package corpus

import "mochi/compiler2/ir"

// BuildMandelbrotKernel builds the mandelbrot BG kernel as described in
// MEP-37 §3.2 (FMA in the iteration) / §3.3 (U8 bitmap output), updated
// for MEP-39 §6.2 iter 2 to dispatch the entire per-pixel iteration
// through a single OpMandelbrotKernel super-op.
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
// Iteration 2 (MEP-39 §6.2) collapses what was a four-IR-function
// recursive descent (rowLoop -> colLoop -> iterate, plus sumU8) into
// one OpMandelbrotKernel dispatch. The escape-count summation stays
// as a tail-recursive helper because that loop runs in O(w*h) and is
// well inside the dispatch budget; only the per-pixel inner loop
// (w*h*maxIter total iterations) is hot enough to merit a super-op.
func BuildMandelbrotKernel(w, h, maxIter int64) *ir.Module {
	const sumIdx = 1

	// main: out = newU8Array(w*h); MandelbrotKernel(out, w, h, mi);
	// return sumU8(out, 0, w*h, 0).
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	wv := bMain.ConstI64(w)
	hv := bMain.ConstI64(h)
	miv := bMain.ConstI64(maxIter)
	n := bMain.MulI64(wv, hv)
	out := bMain.NewU8Array(n)
	bMain.MandelbrotKernel(out, wv, hv, miv)
	zero := bMain.ConstI64(0)
	sum := bMain.Call(sumIdx, ir.TI64, out, zero, n, zero)
	bMain.Ret(sum)

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
		bMain.Function(), bS.Function(),
	}, Main: 0}
}
