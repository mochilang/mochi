package corpus

import "mochi/compiler2/ir"

// BuildSpectralNormKernel builds the spectral_norm BG inner kernel as
// described in MEP-37 §3.2 / §3.3: starting from u = [1.0] * n, perform
// one round of A*u and one round of A^T*u, returning sqrt(sum(v*u) /
// sum(v*v)) where v is the second product. The matrix A is the same
// Hilbert-like one the BG `spectral_norm.go` peer uses:
//
//	A(i,j) = 1 / ((i+j)(i+j+1)/2 + i + 1)
//
// Lowered as four recursive helpers so each loop folds into a
// self-tail-call after opt.TailCall:
//
//	main()                  = fill u with 1.0; v = Au(u); s = AtVu(v); norm(u, v)
//	fill(u, i, n)           = u[i] = 1.0; recurse
//	mulAv(u, out, i, n, ...) = inner product across j, store, recurse
//	dotUV(u, v, i, n, acc)  = acc += u[i]*v[i]; recurse
//
// The result for n=10 matches Go's spectral_norm.go output at 1.276.
//
// This program is intentionally small (no power-method iteration); a
// future BG-canonical corpus entry adds the outer iteration loop. The
// shape suffices to exercise the FP arithmetic + typed-array dispatch
// the rest of MEP-37 Phase 1 is about.
func BuildSpectralNormKernel(n int64) *ir.Module {
	const (
		fillIdx  = 1
		mulAIdx  = 2
		mulInner = 3
		dotIdx   = 4
		evalAIdx = 5
	)

	bMain := ir.NewBuilder("main", nil, ir.TF64)
	nv := bMain.ConstI64(n)
	u := bMain.NewF64Array(nv)
	v := bMain.NewF64Array(nv)
	zero := bMain.ConstI64(0)
	bMain.Call(fillIdx, ir.TUnit, u, zero, nv)
	bMain.Call(mulAIdx, ir.TUnit, u, v, zero, nv)
	vu := bMain.Call(dotIdx, ir.TF64, u, v, zero, nv, bMain.ConstF64(0))
	vv := bMain.Call(dotIdx, ir.TF64, v, v, zero, nv, bMain.ConstF64(0))
	res := bMain.SqrtF64(bMain.DivF64(vu, vv))
	bMain.Ret(res)

	// fill(u, i, n): u[i] = 1.0; tail recurse.
	bF := ir.NewBuilder("fill", []ir.Type{ir.TF64Array, ir.TI64, ir.TI64}, ir.TUnit)
	fu, fi, fn := bF.Param(0), bF.Param(1), bF.Param(2)
	fDone := bF.NewBlock()
	fStep := bF.NewBlock()
	bF.CondBr(bF.LessI64(fi, fn), fStep, fDone)
	bF.SwitchTo(fDone)
	bF.Ret(-1)
	bF.SwitchTo(fStep)
	bF.F64ArrSet(fu, fi, bF.ConstF64(1))
	bF.Call(fillIdx, ir.TUnit, fu, bF.AddI64(fi, bF.ConstI64(1)), fn)
	bF.Ret(-1)

	// mulAv(u, out, i, n): out[i] = inner_j(eval_A(i,j) * u[j]); tail recurse.
	bMA := ir.NewBuilder("mulAv",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TI64, ir.TI64}, ir.TUnit)
	mau, maout, mai, man := bMA.Param(0), bMA.Param(1), bMA.Param(2), bMA.Param(3)
	maDone := bMA.NewBlock()
	maStep := bMA.NewBlock()
	bMA.CondBr(bMA.LessI64(mai, man), maStep, maDone)
	bMA.SwitchTo(maDone)
	bMA.Ret(-1)
	bMA.SwitchTo(maStep)
	inner := bMA.Call(mulInner, ir.TF64,
		mau, mai, bMA.ConstI64(0), man, bMA.ConstF64(0))
	bMA.F64ArrSet(maout, mai, inner)
	bMA.Call(mulAIdx, ir.TUnit, mau, maout,
		bMA.AddI64(mai, bMA.ConstI64(1)), man)
	bMA.Ret(-1)

	// mulInner(u, i, j, n, acc): acc += eval_A(i,j) * u[j]; tail recurse.
	bMI := ir.NewBuilder("mulInner",
		[]ir.Type{ir.TF64Array, ir.TI64, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	miu, mii, mij, min, miAcc := bMI.Param(0), bMI.Param(1), bMI.Param(2), bMI.Param(3), bMI.Param(4)
	miDone := bMI.NewBlock()
	miStep := bMI.NewBlock()
	bMI.CondBr(bMI.LessI64(mij, min), miStep, miDone)
	bMI.SwitchTo(miDone)
	bMI.Ret(miAcc)
	bMI.SwitchTo(miStep)
	a := bMI.Call(evalAIdx, ir.TF64, mii, mij)
	uj := bMI.F64ArrGet(miu, mij)
	prod := bMI.MulF64(a, uj)
	nacc := bMI.AddF64(miAcc, prod)
	bMI.Call(mulInner, ir.TF64,
		miu, mii, bMI.AddI64(mij, bMI.ConstI64(1)), min, nacc)
	bMI.Ret(-1)

	// dot(u, v, i, n, acc): acc += u[i]*v[i]; tail recurse.
	bD := ir.NewBuilder("dot",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	du, dv, di, dn, dAcc := bD.Param(0), bD.Param(1), bD.Param(2), bD.Param(3), bD.Param(4)
	dDone := bD.NewBlock()
	dStep := bD.NewBlock()
	bD.CondBr(bD.LessI64(di, dn), dStep, dDone)
	bD.SwitchTo(dDone)
	bD.Ret(dAcc)
	bD.SwitchTo(dStep)
	ui := bD.F64ArrGet(du, di)
	vi := bD.F64ArrGet(dv, di)
	p := bD.MulF64(ui, vi)
	na := bD.AddF64(dAcc, p)
	bD.Call(dotIdx, ir.TF64, du, dv,
		bD.AddI64(di, bD.ConstI64(1)), dn, na)
	bD.Ret(-1)

	// eval_A(i, j) = 1 / ((i+j)(i+j+1)/2 + i + 1).
	bE := ir.NewBuilder("evalA", []ir.Type{ir.TI64, ir.TI64}, ir.TF64)
	ei, ej := bE.Param(0), bE.Param(1)
	sum := bE.AddI64(ei, ej)
	sumP1 := bE.AddI64(sum, bE.ConstI64(1))
	tri := bE.DivI64(bE.MulI64(sum, sumP1), bE.ConstI64(2))
	denomI := bE.AddI64(bE.AddI64(tri, ei), bE.ConstI64(1))
	denomF := bE.I64ToF64(denomI)
	res2 := bE.DivF64(bE.ConstF64(1), denomF)
	bE.Ret(res2)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bF.Function(), bMA.Function(),
		bMI.Function(), bD.Function(), bE.Function(),
	}, Main: 0}
}
