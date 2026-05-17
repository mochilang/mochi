package corpus

import (
	"math"

	"mochi/compiler2/ir"
)

// Canonical Benchmarks Game n_body initial conditions: the Sun plus the
// four gas giants of the solar system, with the Sun's velocity offset
// so the total momentum is zero. These are the same constants used by
// every BG n_body submission (Go, C, Rust, etc.), which guarantees the
// orbit is bounded and the energy stays in a narrow range across many
// thousands of steps. The chaotic colinear init used in an earlier
// draft of this kernel diverged after ~450 steps, which made cross-lang
// comparison impossible.
const (
	nbodyN         = 5
	nbodyDT        = 0.01
	nbodySolarMass = 4 * math.Pi * math.Pi
	nbodyDaysYear  = 365.24
)

var (
	// pos/vel/mass in BG's astronomical units (AU, AU/day, solar masses
	// scaled by 4π²). Index 0 is the Sun; 1..4 are Jupiter, Saturn,
	// Uranus, Neptune.
	nbodyPosX = [nbodyN]float64{
		0,
		4.84143144246472090e+00,
		8.34336671824457987e+00,
		1.28943695621391310e+01,
		1.53796971148509165e+01,
	}
	nbodyPosY = [nbodyN]float64{
		0,
		-1.16032004402742839e+00,
		4.12479856412430479e+00,
		-1.51111514016986312e+01,
		-2.59193146099879641e+01,
	}
	nbodyPosZ = [nbodyN]float64{
		0,
		-1.03622044471123109e-01,
		-4.03523417114321381e-01,
		-2.23307578892655734e-01,
		1.79258772950371181e-01,
	}
	nbodyVelX = [nbodyN]float64{
		0,
		1.66007664274403694e-03 * nbodyDaysYear,
		-2.76742510726862411e-03 * nbodyDaysYear,
		2.96460137564761618e-03 * nbodyDaysYear,
		2.68067772490389322e-03 * nbodyDaysYear,
	}
	nbodyVelY = [nbodyN]float64{
		0,
		7.69901118419740425e-03 * nbodyDaysYear,
		4.99852801234917238e-03 * nbodyDaysYear,
		2.37847173959480950e-03 * nbodyDaysYear,
		1.62824170038242295e-03 * nbodyDaysYear,
	}
	nbodyVelZ = [nbodyN]float64{
		0,
		-6.90460016972063023e-05 * nbodyDaysYear,
		2.30417297573763929e-05 * nbodyDaysYear,
		-2.96589568540237556e-05 * nbodyDaysYear,
		-9.51592254519715870e-05 * nbodyDaysYear,
	}
	nbodyMass = [nbodyN]float64{
		nbodySolarMass,
		9.54791938424326609e-04 * nbodySolarMass,
		2.85885980666130812e-04 * nbodySolarMass,
		4.36624404335156298e-05 * nbodySolarMass,
		5.15138902046611451e-05 * nbodySolarMass,
	}
)

// nbodyOffsetSunVel returns the velocity components the Sun must have
// so that sum(m_i * v_i) = 0 across all five bodies. The offset is
// computed at module-build time so it can be embedded as a constant in
// the IR, which means the cross-lang peers do not have to replay the
// momentum-correction loop in their startup.
func nbodyOffsetSunVel() (vx, vy, vz float64) {
	for i := 1; i < nbodyN; i++ {
		vx -= nbodyVelX[i] * nbodyMass[i]
		vy -= nbodyVelY[i] * nbodyMass[i]
		vz -= nbodyVelZ[i] * nbodyMass[i]
	}
	return vx / nbodySolarMass, vy / nbodySolarMass, vz / nbodySolarMass
}

// BuildNBody is the parameterised entry point used by the cross-lang
// harness. N is the number of advance + posUpdate iterations; the
// system is the canonical BG five-body solar configuration. The final
// total energy is multiplied by 1e9 and truncated to i64 (toward zero)
// so the harness can integer-compare every language without f64
// stringification quirks.
//
// 1e9 was chosen because the BG energy magnitude is on the order of
// 10^-1 and changes by ~10^-9 over thousands of steps; multiplying by
// 1e9 lands the integer answer in the 10^8 range with one unit per
// nanounit of action, large enough that two IEEE-754-compliant peers
// produce the same integer at every step count we benchmark.
func BuildNBody(steps int64) *ir.Module {
	const (
		mainIdx      = 0
		advanceIdx   = 1
		innerIdx     = 2
		posIdx       = 3
		energyIdx    = 4
		subEnergyIdx = 5
		stepLoopIdx  = 6
	)

	sunVX, sunVY, sunVZ := nbodyOffsetSunVel()
	nv := int64(nbodyN)

	// main(): allocate arrays, store the 35 init constants inline,
	// install the offset-momentum Sun velocity, run stepLoop, compute
	// final energy, quantize to i64.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nConst := bMain.ConstI64(nv)
	x := bMain.NewF64Array(nConst)
	y := bMain.NewF64Array(nConst)
	z := bMain.NewF64Array(nConst)
	vx := bMain.NewF64Array(nConst)
	vy := bMain.NewF64Array(nConst)
	vz := bMain.NewF64Array(nConst)
	m := bMain.NewF64Array(nConst)
	for i := int64(0); i < nv; i++ {
		idx := bMain.ConstI64(i)
		bMain.F64ArrSet(x, idx, bMain.ConstF64(nbodyPosX[i]))
		bMain.F64ArrSet(y, idx, bMain.ConstF64(nbodyPosY[i]))
		bMain.F64ArrSet(z, idx, bMain.ConstF64(nbodyPosZ[i]))
		if i == 0 {
			bMain.F64ArrSet(vx, idx, bMain.ConstF64(sunVX))
			bMain.F64ArrSet(vy, idx, bMain.ConstF64(sunVY))
			bMain.F64ArrSet(vz, idx, bMain.ConstF64(sunVZ))
		} else {
			bMain.F64ArrSet(vx, idx, bMain.ConstF64(nbodyVelX[i]))
			bMain.F64ArrSet(vy, idx, bMain.ConstF64(nbodyVelY[i]))
			bMain.F64ArrSet(vz, idx, bMain.ConstF64(nbodyVelZ[i]))
		}
		bMain.F64ArrSet(m, idx, bMain.ConstF64(nbodyMass[i]))
	}
	zero := bMain.ConstI64(0)
	dtv := bMain.ConstF64(nbodyDT)
	bMain.Call(stepLoopIdx, ir.TUnit, x, y, z, vx, vy, vz, m,
		zero, bMain.ConstI64(steps), dtv)
	e := bMain.Call(energyIdx, ir.TF64, x, y, z, vx, vy, vz, m, zero,
		bMain.ConstF64(0))
	scaled := bMain.MulF64(e, bMain.ConstF64(1e9))
	bMain.Ret(bMain.F64ToI64(scaled))

	// advance(arrays, i, dt): for each i<N call inner(i, i+1) then
	// recurse to i+1. inner walks j across (i, N).
	bA := ir.NewBuilder("advance",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TF64}, ir.TUnit)
	ax, ay, az, avx, avy, avz, am, ai, adt :=
		bA.Param(0), bA.Param(1), bA.Param(2), bA.Param(3),
		bA.Param(4), bA.Param(5), bA.Param(6), bA.Param(7), bA.Param(8)
	aDone := bA.NewBlock()
	aStep := bA.NewBlock()
	bA.CondBr(bA.LessI64(ai, bA.ConstI64(nv)), aStep, aDone)
	bA.SwitchTo(aDone)
	bA.Ret(-1)
	bA.SwitchTo(aStep)
	bA.Call(innerIdx, ir.TUnit, ax, ay, az, avx, avy, avz, am, ai,
		bA.AddI64(ai, bA.ConstI64(1)), adt)
	bA.Call(advanceIdx, ir.TUnit, ax, ay, az, avx, avy, avz, am,
		bA.AddI64(ai, bA.ConstI64(1)), adt)
	bA.Ret(-1)

	// inner(arrays, i, j, dt): pairwise velocity update of bodies i,j.
	bN := ir.NewBuilder("inner",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TUnit)
	nx, ny, nz, nvx, nvy, nvz, nm, ni, nj, ndt :=
		bN.Param(0), bN.Param(1), bN.Param(2), bN.Param(3),
		bN.Param(4), bN.Param(5), bN.Param(6), bN.Param(7),
		bN.Param(8), bN.Param(9)
	nDone := bN.NewBlock()
	nStep := bN.NewBlock()
	bN.CondBr(bN.LessI64(nj, bN.ConstI64(nv)), nStep, nDone)
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
	vxi := bN.F64ArrGet(nvx, ni)
	bN.F64ArrSet(nvx, ni, bN.SubF64(vxi, bN.MulF64(dx, mjMag)))
	vyi := bN.F64ArrGet(nvy, ni)
	bN.F64ArrSet(nvy, ni, bN.SubF64(vyi, bN.MulF64(dy, mjMag)))
	vzi := bN.F64ArrGet(nvz, ni)
	bN.F64ArrSet(nvz, ni, bN.SubF64(vzi, bN.MulF64(dz, mjMag)))
	vxj := bN.F64ArrGet(nvx, nj)
	bN.F64ArrSet(nvx, nj, bN.AddF64(vxj, bN.MulF64(dx, miMag)))
	vyj := bN.F64ArrGet(nvy, nj)
	bN.F64ArrSet(nvy, nj, bN.AddF64(vyj, bN.MulF64(dy, miMag)))
	vzj := bN.F64ArrGet(nvz, nj)
	bN.F64ArrSet(nvz, nj, bN.AddF64(vzj, bN.MulF64(dz, miMag)))
	bN.Call(innerIdx, ir.TUnit, nx, ny, nz, nvx, nvy, nvz, nm, ni,
		bN.AddI64(nj, bN.ConstI64(1)), ndt)
	bN.Ret(-1)

	// posUpdate(arrays, i, dt): position += velocity * dt for body i.
	bP := ir.NewBuilder("posUpdate",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TI64, ir.TF64}, ir.TUnit)
	px, py, pz, pvx, pvy, pvz, pi, pdt :=
		bP.Param(0), bP.Param(1), bP.Param(2), bP.Param(3),
		bP.Param(4), bP.Param(5), bP.Param(6), bP.Param(7)
	pDone := bP.NewBlock()
	pStep := bP.NewBlock()
	bP.CondBr(bP.LessI64(pi, bP.ConstI64(nv)), pStep, pDone)
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

	// energy(arrays, i, acc): kinetic - pairwise potential for body i,
	// recursed across all bodies. The accumulator is threaded through
	// so the leaf return delivers the total in one f64.
	bE := ir.NewBuilder("energy",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TF64}, ir.TF64)
	ex, ey, ez, evx, evy, evz, em, ei, eAcc :=
		bE.Param(0), bE.Param(1), bE.Param(2), bE.Param(3),
		bE.Param(4), bE.Param(5), bE.Param(6), bE.Param(7), bE.Param(8)
	eDone := bE.NewBlock()
	eStep := bE.NewBlock()
	bE.CondBr(bE.LessI64(ei, bE.ConstI64(nv)), eStep, eDone)
	bE.SwitchTo(eDone)
	bE.Ret(eAcc)
	bE.SwitchTo(eStep)
	evxi := bE.F64ArrGet(evx, ei)
	evyi := bE.F64ArrGet(evy, ei)
	evzi := bE.F64ArrGet(evz, ei)
	vSq := bE.AddF64(bE.AddF64(bE.MulF64(evxi, evxi), bE.MulF64(evyi, evyi)), bE.MulF64(evzi, evzi))
	emi := bE.F64ArrGet(em, ei)
	kin := bE.MulF64(bE.MulF64(bE.ConstF64(0.5), emi), vSq)
	pot := bE.Call(subEnergyIdx, ir.TF64, ex, ey, ez, em, ei,
		bE.AddI64(ei, bE.ConstI64(1)), bE.ConstF64(0))
	delta := bE.SubF64(kin, pot)
	nacc := bE.AddF64(eAcc, delta)
	er := bE.Call(energyIdx, ir.TF64, ex, ey, ez, evx, evy, evz, em,
		bE.AddI64(ei, bE.ConstI64(1)), nacc)
	bE.Ret(er)

	// subEnergy(arrays, i, j, acc): subtract m[i]*m[j]/r contribution.
	bS := ir.NewBuilder("subEnergy",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TF64)
	sx, sy, sz, sm, si, sj, sAcc :=
		bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3),
		bS.Param(4), bS.Param(5), bS.Param(6)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sj, bS.ConstI64(nv)), sStep, sDone)
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

	// stepLoop(arrays, k, steps, dt): one advance + one posUpdate,
	// then recurse to k+1. The tail call is a 3-arg self-call so it
	// hits the OpTailCallSelfA3 fast path once it lands.
	bL := ir.NewBuilder("stepLoop",
		[]ir.Type{ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TF64Array, ir.TF64Array,
			ir.TF64Array, ir.TI64, ir.TI64, ir.TF64}, ir.TUnit)
	lx, ly, lz, lvx, lvy, lvz, lm, lk, lsteps, ldt :=
		bL.Param(0), bL.Param(1), bL.Param(2), bL.Param(3),
		bL.Param(4), bL.Param(5), bL.Param(6), bL.Param(7),
		bL.Param(8), bL.Param(9)
	lDone := bL.NewBlock()
	lStep := bL.NewBlock()
	bL.CondBr(bL.LessI64(lk, lsteps), lStep, lDone)
	bL.SwitchTo(lDone)
	bL.Ret(-1)
	bL.SwitchTo(lStep)
	bL.Call(advanceIdx, ir.TUnit, lx, ly, lz, lvx, lvy, lvz, lm,
		bL.ConstI64(0), ldt)
	bL.Call(posIdx, ir.TUnit, lx, ly, lz, lvx, lvy, lvz,
		bL.ConstI64(0), ldt)
	bL.Call(stepLoopIdx, ir.TUnit, lx, ly, lz, lvx, lvy, lvz, lm,
		bL.AddI64(lk, bL.ConstI64(1)), lsteps, ldt)
	bL.Ret(-1)

	_ = mainIdx
	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bA.Function(), bN.Function(),
		bP.Function(), bE.Function(), bS.Function(), bL.Function(),
	}, Main: 0}
}

// ExpectNBody mirrors BuildNBody bit-for-bit so the corpus oracle stays
// self-checking. Every peer template (mochi, py, lua, luajit, go) uses
// the same op order so all IEEE-754 conforming runtimes produce the
// same i64.
func ExpectNBody(steps int64) int64 {
	x := make([]float64, nbodyN)
	y := make([]float64, nbodyN)
	z := make([]float64, nbodyN)
	vx := make([]float64, nbodyN)
	vy := make([]float64, nbodyN)
	vz := make([]float64, nbodyN)
	m := make([]float64, nbodyN)
	sunVX, sunVY, sunVZ := nbodyOffsetSunVel()
	for i := 0; i < nbodyN; i++ {
		x[i] = nbodyPosX[i]
		y[i] = nbodyPosY[i]
		z[i] = nbodyPosZ[i]
		if i == 0 {
			vx[i] = sunVX
			vy[i] = sunVY
			vz[i] = sunVZ
		} else {
			vx[i] = nbodyVelX[i]
			vy[i] = nbodyVelY[i]
			vz[i] = nbodyVelZ[i]
		}
		m[i] = nbodyMass[i]
	}
	for k := int64(0); k < steps; k++ {
		for i := 0; i < nbodyN; i++ {
			for j := i + 1; j < nbodyN; j++ {
				dx := x[i] - x[j]
				dy := y[i] - y[j]
				dz := z[i] - z[j]
				d2 := dx*dx + dy*dy + dz*dz
				mag := nbodyDT / (d2 * math.Sqrt(d2))
				miMag := m[i] * mag
				mjMag := m[j] * mag
				vx[i] -= dx * mjMag
				vy[i] -= dy * mjMag
				vz[i] -= dz * mjMag
				vx[j] += dx * miMag
				vy[j] += dy * miMag
				vz[j] += dz * miMag
			}
		}
		for i := 0; i < nbodyN; i++ {
			x[i] += vx[i] * nbodyDT
			y[i] += vy[i] * nbodyDT
			z[i] += vz[i] * nbodyDT
		}
	}
	var e float64
	for i := 0; i < nbodyN; i++ {
		kin := 0.5 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i])
		var pot float64
		for j := i + 1; j < nbodyN; j++ {
			dx := x[i] - x[j]
			dy := y[i] - y[j]
			dz := z[i] - z[j]
			r := math.Sqrt(dx*dx + dy*dy + dz*dz)
			pot += m[i] * m[j] / r
		}
		e += kin - pot
	}
	return int64(e * 1e9)
}
