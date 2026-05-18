package corpus

import (
	"math"

	"mochi/compiler2/ir"
)

// BuildSpectralNorm builds the canonical Benchmarks Game spectral_norm
// program as the scaled cross-lang companion to BuildSpectralNormKernel.
// The kernel form did only one round of Au + At*v with a fixed vector
// length; this form runs the full BG power method (10 iterations of
// AtAu) over an N-element vector and returns floor(sqrt(uBu/uu) * 1e9)
// so the crosslang harness can compare integer values without f64
// stringification (matching the same idiom as BuildNBody).
//
// Matrix A is the Hilbert-like matrix used by every BG submission:
//
//	A(i, j) = 1 / ((i + j) * (i + j + 1) / 2 + i + 1)
//
// Power-method loop (10 iterations = 5 pairs):
//
//	tmp = A  * u   ; v = At * tmp   // v = AtAu(u)
//	tmp = A  * v   ; u = At * tmp   // u = AtAu(v)
//
// Final value: sqrt(sum(u*v) / sum(v*v)).
//
// All five peers (vm2, Go, Python, Lua, LuaJIT) produce bit-identical
// int64 output at every N we test (subject to IEEE-754 round-to-nearest
// behaving the same in each runtime, which it does for the magnitudes
// here).
func BuildSpectralNorm(n int64) *ir.Module {
	const (
		fillIdx     = 1
		mulAIdx     = 2
		mulAInner   = 3
		mulAtIdx    = 4
		mulAtInner  = 5
		dotIdx      = 6
		iterIdx     = 7
		evalAIdx    = 8
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	u := bMain.NewF64Array(nv)
	v := bMain.NewF64Array(nv)
	tmp := bMain.NewF64Array(nv)
	zero := bMain.ConstI64(0)
	bMain.Call(fillIdx, ir.TUnit, u, zero, nv)
	bMain.Call(iterIdx, ir.TUnit, u, v, tmp, zero, bMain.ConstI64(5), nv)
	uv := bMain.Call(dotIdx, ir.TF64, u, v, zero, nv, bMain.ConstF64(0))
	vv := bMain.Call(dotIdx, ir.TF64, v, v, zero, nv, bMain.ConstF64(0))
	res := bMain.SqrtF64(bMain.DivF64(uv, vv))
	scaled := bMain.MulF64(res, bMain.ConstF64(1e9))
	bMain.Ret(bMain.F64ToI64(scaled))

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

	// mulAv(u, out, i, n): out[i] = sum_j A(i,j) * u[j]; tail recurse.
	bMA := ir.NewBuilder("mulAv",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TI64, ir.TI64}, ir.TUnit)
	mau, maout, mai, man := bMA.Param(0), bMA.Param(1), bMA.Param(2), bMA.Param(3)
	maDone := bMA.NewBlock()
	maStep := bMA.NewBlock()
	bMA.CondBr(bMA.LessI64(mai, man), maStep, maDone)
	bMA.SwitchTo(maDone)
	bMA.Ret(-1)
	bMA.SwitchTo(maStep)
	inner := bMA.Call(mulAInner, ir.TF64,
		mau, mai, bMA.ConstI64(0), man, bMA.ConstF64(0))
	bMA.F64ArrSet(maout, mai, inner)
	bMA.Call(mulAIdx, ir.TUnit, mau, maout,
		bMA.AddI64(mai, bMA.ConstI64(1)), man)
	bMA.Ret(-1)

	// mulAInner(u, i, j, n, acc): acc += A(i,j) * u[j]; tail recurse.
	bMI := ir.NewBuilder("mulAInner",
		[]ir.Type{ir.TF64Array, ir.TI64, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	miu, mii, mij, miN, miAcc := bMI.Param(0), bMI.Param(1), bMI.Param(2), bMI.Param(3), bMI.Param(4)
	miDone := bMI.NewBlock()
	miStep := bMI.NewBlock()
	bMI.CondBr(bMI.LessI64(mij, miN), miStep, miDone)
	bMI.SwitchTo(miDone)
	bMI.Ret(miAcc)
	bMI.SwitchTo(miStep)
	a := bMI.Call(evalAIdx, ir.TF64, mii, mij)
	uj := bMI.F64ArrGet(miu, mij)
	prod := bMI.MulF64(a, uj)
	nacc := bMI.AddF64(miAcc, prod)
	rmi := bMI.Call(mulAInner, ir.TF64,
		miu, mii, bMI.AddI64(mij, bMI.ConstI64(1)), miN, nacc)
	bMI.Ret(rmi)

	// mulAtv(u, out, i, n): out[i] = sum_j A(j,i) * u[j]; tail recurse.
	bAT := ir.NewBuilder("mulAtv",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TI64, ir.TI64}, ir.TUnit)
	atu, atout, ati, atn := bAT.Param(0), bAT.Param(1), bAT.Param(2), bAT.Param(3)
	atDone := bAT.NewBlock()
	atStep := bAT.NewBlock()
	bAT.CondBr(bAT.LessI64(ati, atn), atStep, atDone)
	bAT.SwitchTo(atDone)
	bAT.Ret(-1)
	bAT.SwitchTo(atStep)
	innerAt := bAT.Call(mulAtInner, ir.TF64,
		atu, ati, bAT.ConstI64(0), atn, bAT.ConstF64(0))
	bAT.F64ArrSet(atout, ati, innerAt)
	bAT.Call(mulAtIdx, ir.TUnit, atu, atout,
		bAT.AddI64(ati, bAT.ConstI64(1)), atn)
	bAT.Ret(-1)

	// mulAtInner(u, i, j, n, acc): acc += A(j,i) * u[j]; tail recurse.
	// Note swapped (j, i) into evalA so this routine uses At rather than A.
	bATI := ir.NewBuilder("mulAtInner",
		[]ir.Type{ir.TF64Array, ir.TI64, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	atiu, atii, atij, atin, atiAcc := bATI.Param(0), bATI.Param(1), bATI.Param(2), bATI.Param(3), bATI.Param(4)
	atiDone := bATI.NewBlock()
	atiStep := bATI.NewBlock()
	bATI.CondBr(bATI.LessI64(atij, atin), atiStep, atiDone)
	bATI.SwitchTo(atiDone)
	bATI.Ret(atiAcc)
	bATI.SwitchTo(atiStep)
	aT := bATI.Call(evalAIdx, ir.TF64, atij, atii)
	ujT := bATI.F64ArrGet(atiu, atij)
	prodT := bATI.MulF64(aT, ujT)
	naccT := bATI.AddF64(atiAcc, prodT)
	rati := bATI.Call(mulAtInner, ir.TF64,
		atiu, atii, bATI.AddI64(atij, bATI.ConstI64(1)), atin, naccT)
	bATI.Ret(rati)

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
	rd := bD.Call(dotIdx, ir.TF64, du, dv,
		bD.AddI64(di, bD.ConstI64(1)), dn, na)
	bD.Ret(rd)

	// iterLoop(u, v, tmp, k, total, n): each step does one pair of
	//   tmp = A*u; v = At*tmp     (v = AtAu(u))
	//   tmp = A*v; u = At*tmp     (u = AtAu(v))
	// so total=5 produces the canonical BG 10-iteration power method.
	bI := ir.NewBuilder("iterLoop",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array, ir.TI64, ir.TI64, ir.TI64}, ir.TUnit)
	iu, iv, itmp, ik, itot, inN := bI.Param(0), bI.Param(1), bI.Param(2), bI.Param(3), bI.Param(4), bI.Param(5)
	iDone := bI.NewBlock()
	iStep := bI.NewBlock()
	bI.CondBr(bI.LessI64(ik, itot), iStep, iDone)
	bI.SwitchTo(iDone)
	bI.Ret(-1)
	bI.SwitchTo(iStep)
	bI.Call(mulAIdx, ir.TUnit, iu, itmp, bI.ConstI64(0), inN)
	bI.Call(mulAtIdx, ir.TUnit, itmp, iv, bI.ConstI64(0), inN)
	bI.Call(mulAIdx, ir.TUnit, iv, itmp, bI.ConstI64(0), inN)
	bI.Call(mulAtIdx, ir.TUnit, itmp, iu, bI.ConstI64(0), inN)
	bI.Call(iterIdx, ir.TUnit, iu, iv, itmp, bI.AddI64(ik, bI.ConstI64(1)), itot, inN)
	bI.Ret(-1)

	// eval_A(i, j) = 1 / ((i+j)(i+j+1)/2 + i + 1)
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
		bMain.Function(),
		bF.Function(),
		bMA.Function(), bMI.Function(),
		bAT.Function(), bATI.Function(),
		bD.Function(),
		bI.Function(),
		bE.Function(),
	}, Main: 0}
}

// ExpectSpectralNorm runs the same algorithm in plain Go so the oracle
// test can assert vm2 produces the bit-identical int64 result.
func ExpectSpectralNorm(n int64) int64 {
	u := make([]float64, n)
	v := make([]float64, n)
	tmp := make([]float64, n)
	for i := range u {
		u[i] = 1.0
	}
	A := func(i, j int64) float64 {
		s := i + j
		return 1.0 / float64(s*(s+1)/2+i+1)
	}
	mulAv := func(src, dst []float64) {
		for i := int64(0); i < n; i++ {
			sum := 0.0
			for j := int64(0); j < n; j++ {
				sum += A(i, j) * src[j]
			}
			dst[i] = sum
		}
	}
	mulAtv := func(src, dst []float64) {
		for i := int64(0); i < n; i++ {
			sum := 0.0
			for j := int64(0); j < n; j++ {
				sum += A(j, i) * src[j]
			}
			dst[i] = sum
		}
	}
	for k := 0; k < 5; k++ {
		mulAv(u, tmp)
		mulAtv(tmp, v)
		mulAv(v, tmp)
		mulAtv(tmp, u)
	}
	uv, vv := 0.0, 0.0
	for i := int64(0); i < n; i++ {
		uv += u[i] * v[i]
		vv += v[i] * v[i]
	}
	return int64(math.Sqrt(uv/vv) * 1e9)
}
