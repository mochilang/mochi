package corpus

import "mochi/compiler2/ir"

// BuildNBodyKernel builds the BG `n_body` per-step kernel as described
// in MEP-37 §3.2: 5 bodies with position/velocity/mass arrays, one
// advance() step at dt=0.01, then return the total system energy
// (kinetic minus pairwise potential).
//
// The canonical BG n_body iterates the advance step 50_000_000 times.
// The kernel here runs a single iteration; the BG number scales
// linearly with iteration count so one-step timings extrapolate
// cleanly. Initial conditions are simple test values (positions
// (i, 2i, 3i), velocities (i/10, i/5, 3i/10), masses 1..5) rather
// than the planetary defaults — this keeps the Go oracle small and
// the test deterministic.
//
// Lowered as eight functions so each loop folds into a self-tail-call
// after opt.TailCall:
//
//	main             = init arrays + advance + energy
//	initFn(arrs, i)  = fill arrays at index i; tail recurse
//	advance(arrs,i)  = inner(arrs, i, i+1); tail recurse
//	inner(arrs,i,j)  = update vi/vj from pairwise force; tail recurse
//	posUpdate(arrs,i)= xi += vxi*dt; tail recurse
//	energy(arrs,i,acc)= kin - pot to acc; tail recurse
//	subEnergy(arrs,i,j,acc) = subtract pairwise potential; tail recurse
//
// The result is verified end-to-end against a Go implementation in
// runtime/vm2/bench/bg_n_body_test.go.
func BuildNBodyKernel() *ir.Module {
	const (
		initIdx     = 1
		advanceIdx  = 2
		innerIdx    = 3
		posIdx      = 4
		energyIdx   = 5
		subEnergyIdx = 6
		N           = int64(5)
	)
	dt := 0.01

	// main()
	bMain := ir.NewBuilder("main", nil, ir.TF64)
	nv := bMain.ConstI64(N)
	x := bMain.NewF64Array(nv)
	y := bMain.NewF64Array(nv)
	z := bMain.NewF64Array(nv)
	vx := bMain.NewF64Array(nv)
	vy := bMain.NewF64Array(nv)
	vz := bMain.NewF64Array(nv)
	m := bMain.NewF64Array(nv)
	zero := bMain.ConstI64(0)
	bMain.Call(initIdx, ir.TUnit, x, y, z, vx, vy, vz, m, zero)
	dtv := bMain.ConstF64(dt)
	bMain.Call(advanceIdx, ir.TUnit, x, y, z, vx, vy, vz, m, zero, dtv)
	bMain.Call(posIdx, ir.TUnit, x, y, z, vx, vy, vz, zero, dtv)
	e := bMain.Call(energyIdx, ir.TF64, x, y, z, vx, vy, vz, m, zero, bMain.ConstF64(0))
	bMain.Ret(e)

	// initFn(x, y, z, vx, vy, vz, m, i)
	bI := ir.NewBuilder("init",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64}, ir.TUnit)
	bx, by, bz, bvx, bvy, bvz, bm, bi := bI.Param(0), bI.Param(1), bI.Param(2), bI.Param(3), bI.Param(4), bI.Param(5), bI.Param(6), bI.Param(7)
	iDone := bI.NewBlock()
	iStep := bI.NewBlock()
	bI.CondBr(bI.LessI64(bi, bI.ConstI64(N)), iStep, iDone)
	bI.SwitchTo(iDone)
	bI.Ret(-1)
	bI.SwitchTo(iStep)
	fi := bI.I64ToF64(bi)
	bI.F64ArrSet(bx, bi, fi)
	bI.F64ArrSet(by, bi, bI.MulF64(fi, bI.ConstF64(2)))
	bI.F64ArrSet(bz, bi, bI.MulF64(fi, bI.ConstF64(3)))
	bI.F64ArrSet(bvx, bi, bI.MulF64(fi, bI.ConstF64(0.1)))
	bI.F64ArrSet(bvy, bi, bI.MulF64(fi, bI.ConstF64(0.2)))
	bI.F64ArrSet(bvz, bi, bI.MulF64(fi, bI.ConstF64(0.3)))
	// mass = i+1
	bI.F64ArrSet(bm, bi, bI.AddF64(fi, bI.ConstF64(1)))
	bI.Call(initIdx, ir.TUnit, bx, by, bz, bvx, bvy, bvz, bm,
		bI.AddI64(bi, bI.ConstI64(1)))
	bI.Ret(-1)

	// advance(x, y, z, vx, vy, vz, m, i, dt)
	bA := ir.NewBuilder("advance",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TF64}, ir.TUnit)
	ax, ay, az, avx, avy, avz, am, ai, adt := bA.Param(0), bA.Param(1), bA.Param(2), bA.Param(3), bA.Param(4), bA.Param(5), bA.Param(6), bA.Param(7), bA.Param(8)
	aDone := bA.NewBlock()
	aStep := bA.NewBlock()
	bA.CondBr(bA.LessI64(ai, bA.ConstI64(N)), aStep, aDone)
	bA.SwitchTo(aDone)
	bA.Ret(-1)
	bA.SwitchTo(aStep)
	bA.Call(innerIdx, ir.TUnit, ax, ay, az, avx, avy, avz, am, ai,
		bA.AddI64(ai, bA.ConstI64(1)), adt)
	bA.Call(advanceIdx, ir.TUnit, ax, ay, az, avx, avy, avz, am,
		bA.AddI64(ai, bA.ConstI64(1)), adt)
	bA.Ret(-1)

	// inner(x, y, z, vx, vy, vz, m, i, j, dt)
	bN := ir.NewBuilder("inner",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TUnit)
	nx, ny, nz, nvx, nvy, nvz, nm, ni, nj, ndt := bN.Param(0), bN.Param(1), bN.Param(2), bN.Param(3), bN.Param(4), bN.Param(5), bN.Param(6), bN.Param(7), bN.Param(8), bN.Param(9)
	nDone := bN.NewBlock()
	nStep := bN.NewBlock()
	bN.CondBr(bN.LessI64(nj, bN.ConstI64(N)), nStep, nDone)
	bN.SwitchTo(nDone)
	bN.Ret(-1)
	bN.SwitchTo(nStep)
	xi := bN.F64ArrGet(nx, ni)
	xj := bN.F64ArrGet(nx, nj)
	dx := bN.SubF64(xi, xj)
	yi := bN.F64ArrGet(ny, ni)
	yj := bN.F64ArrGet(ny, nj)
	dy := bN.SubF64(yi, yj)
	zi := bN.F64ArrGet(nz, ni)
	zj := bN.F64ArrGet(nz, nj)
	dz := bN.SubF64(zi, zj)
	d2 := bN.AddF64(bN.AddF64(bN.MulF64(dx, dx), bN.MulF64(dy, dy)), bN.MulF64(dz, dz))
	mag := bN.DivF64(ndt, bN.MulF64(d2, bN.SqrtF64(d2)))
	mi := bN.F64ArrGet(nm, ni)
	mj := bN.F64ArrGet(nm, nj)
	miMag := bN.MulF64(mi, mag)
	mjMag := bN.MulF64(mj, mag)
	// vi -= d * mj_mag
	vxi := bN.F64ArrGet(nvx, ni)
	bN.F64ArrSet(nvx, ni, bN.SubF64(vxi, bN.MulF64(dx, mjMag)))
	vyi := bN.F64ArrGet(nvy, ni)
	bN.F64ArrSet(nvy, ni, bN.SubF64(vyi, bN.MulF64(dy, mjMag)))
	vzi := bN.F64ArrGet(nvz, ni)
	bN.F64ArrSet(nvz, ni, bN.SubF64(vzi, bN.MulF64(dz, mjMag)))
	// vj += d * mi_mag
	vxj := bN.F64ArrGet(nvx, nj)
	bN.F64ArrSet(nvx, nj, bN.AddF64(vxj, bN.MulF64(dx, miMag)))
	vyj := bN.F64ArrGet(nvy, nj)
	bN.F64ArrSet(nvy, nj, bN.AddF64(vyj, bN.MulF64(dy, miMag)))
	vzj := bN.F64ArrGet(nvz, nj)
	bN.F64ArrSet(nvz, nj, bN.AddF64(vzj, bN.MulF64(dz, miMag)))
	bN.Call(innerIdx, ir.TUnit, nx, ny, nz, nvx, nvy, nvz, nm, ni,
		bN.AddI64(nj, bN.ConstI64(1)), ndt)
	bN.Ret(-1)

	// posUpdate(x, y, z, vx, vy, vz, i, dt)
	bP := ir.NewBuilder("posUpdate",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TI64, ir.TF64}, ir.TUnit)
	px, py, pz, pvx, pvy, pvz, pi, pdt := bP.Param(0), bP.Param(1), bP.Param(2), bP.Param(3), bP.Param(4), bP.Param(5), bP.Param(6), bP.Param(7)
	pDone := bP.NewBlock()
	pStep := bP.NewBlock()
	bP.CondBr(bP.LessI64(pi, bP.ConstI64(N)), pStep, pDone)
	bP.SwitchTo(pDone)
	bP.Ret(-1)
	bP.SwitchTo(pStep)
	pXi := bP.F64ArrGet(px, pi)
	pVxi := bP.F64ArrGet(pvx, pi)
	bP.F64ArrSet(px, pi, bP.AddF64(pXi, bP.MulF64(pVxi, pdt)))
	pYi := bP.F64ArrGet(py, pi)
	pVyi := bP.F64ArrGet(pvy, pi)
	bP.F64ArrSet(py, pi, bP.AddF64(pYi, bP.MulF64(pVyi, pdt)))
	pZi := bP.F64ArrGet(pz, pi)
	pVzi := bP.F64ArrGet(pvz, pi)
	bP.F64ArrSet(pz, pi, bP.AddF64(pZi, bP.MulF64(pVzi, pdt)))
	bP.Call(posIdx, ir.TUnit, px, py, pz, pvx, pvy, pvz,
		bP.AddI64(pi, bP.ConstI64(1)), pdt)
	bP.Ret(-1)

	// energy(x, y, z, vx, vy, vz, m, i, acc)
	bE := ir.NewBuilder("energy",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TF64}, ir.TF64)
	ex, ey, ez, evx, evy, evz, em, ei, eAcc := bE.Param(0), bE.Param(1), bE.Param(2), bE.Param(3), bE.Param(4), bE.Param(5), bE.Param(6), bE.Param(7), bE.Param(8)
	eDone := bE.NewBlock()
	eStep := bE.NewBlock()
	bE.CondBr(bE.LessI64(ei, bE.ConstI64(N)), eStep, eDone)
	bE.SwitchTo(eDone)
	bE.Ret(eAcc)
	bE.SwitchTo(eStep)
	// kin = 0.5 * m[i] * (vx[i]^2 + vy[i]^2 + vz[i]^2)
	evxi := bE.F64ArrGet(evx, ei)
	evyi := bE.F64ArrGet(evy, ei)
	evzi := bE.F64ArrGet(evz, ei)
	vSq := bE.AddF64(bE.AddF64(bE.MulF64(evxi, evxi), bE.MulF64(evyi, evyi)), bE.MulF64(evzi, evzi))
	emi := bE.F64ArrGet(em, ei)
	kin := bE.MulF64(bE.MulF64(bE.ConstF64(0.5), emi), vSq)
	// pot = subEnergy(i+1, 0)
	pot := bE.Call(subEnergyIdx, ir.TF64, ex, ey, ez, em, ei,
		bE.AddI64(ei, bE.ConstI64(1)), bE.ConstF64(0))
	delta := bE.SubF64(kin, pot)
	nacc := bE.AddF64(eAcc, delta)
	er := bE.Call(energyIdx, ir.TF64, ex, ey, ez, evx, evy, evz, em,
		bE.AddI64(ei, bE.ConstI64(1)), nacc)
	bE.Ret(er)

	// subEnergy(x, y, z, m, i, j, acc) -> add m[i]*m[j]/r
	bS := ir.NewBuilder("subEnergy",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	sx, sy, sz, sm, si, sj, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3), bS.Param(4), bS.Param(5), bS.Param(6)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sj, bS.ConstI64(N)), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	sxi := bS.F64ArrGet(sx, si)
	sxj := bS.F64ArrGet(sx, sj)
	sdx := bS.SubF64(sxi, sxj)
	syi := bS.F64ArrGet(sy, si)
	syj := bS.F64ArrGet(sy, sj)
	sdy := bS.SubF64(syi, syj)
	szi := bS.F64ArrGet(sz, si)
	szj := bS.F64ArrGet(sz, sj)
	sdz := bS.SubF64(szi, szj)
	r := bS.SqrtF64(bS.AddF64(bS.AddF64(bS.MulF64(sdx, sdx), bS.MulF64(sdy, sdy)), bS.MulF64(sdz, sdz)))
	smi := bS.F64ArrGet(sm, si)
	smj := bS.F64ArrGet(sm, sj)
	contrib := bS.DivF64(bS.MulF64(smi, smj), r)
	nAcc2 := bS.AddF64(sAcc, contrib)
	sr := bS.Call(subEnergyIdx, ir.TF64, sx, sy, sz, sm, si,
		bS.AddI64(sj, bS.ConstI64(1)), nAcc2)
	bS.Ret(sr)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bI.Function(), bA.Function(),
		bN.Function(), bP.Function(), bE.Function(), bS.Function(),
	}, Main: 0}
}
