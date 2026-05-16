package corpus

import "mochi/compiler2/ir"

// BuildListsFillSum builds:
//
//	xs := []  // allocate empty list with capacity N
//	for i := 0; i < N; i++ { xs.push(i) }
//	s := 0
//	for j := 0; j < N; j++ { s += xs[j] }
//	return s
//
// Encoded as two self-tail-call helpers (one for fill, one for sum)
// so the emitter folds each loop into OpTailCallSelf. The return is
// sum(0..n-1) == n*(n-1)/2 so the runner can assert a single scalar.
func BuildListsFillSum(n int64) *ir.Module {
	const fillIdx = 1
	const sumIdx = 2

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	xs := bMain.NewList(n)
	i0 := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	bMain.Call(fillIdx, ir.TUnit, xs, i0, nv)
	s0 := bMain.ConstI64(0)
	j0 := bMain.ConstI64(0)
	r := bMain.Call(sumIdx, ir.TI64, xs, j0, nv, s0)
	bMain.Ret(r)

	bF := ir.NewBuilder("fill", []ir.Type{ir.TList, ir.TI64, ir.TI64}, ir.TUnit)
	pXs, pI, pN := bF.Param(0), bF.Param(1), bF.Param(2)
	done := bF.NewBlock()
	step := bF.NewBlock()
	bF.CondBr(bF.LessI64(pI, pN), step, done)
	bF.SwitchTo(done)
	bF.Ret(-1)
	bF.SwitchTo(step)
	bF.ListPush(pXs, pI)
	one := bF.ConstI64(1)
	ni := bF.AddI64(pI, one)
	bF.Call(fillIdx, ir.TUnit, pXs, ni, pN)
	bF.Ret(-1)

	bS := ir.NewBuilder("sum", []ir.Type{ir.TList, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sXs, sJ, sN, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sJ, sN), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	x := bS.ListGet(sXs, sJ, ir.TI64)
	nAcc := bS.AddI64(sAcc, x)
	sOne := bS.ConstI64(1)
	nJ := bS.AddI64(sJ, sOne)
	r2 := bS.Call(sumIdx, ir.TI64, sXs, nJ, sN, nAcc)
	bS.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bF.Function(), bS.Function()}, Main: 0}
}

// ExpectListsFillSum returns the sum 0+1+...+(n-1) == n*(n-1)/2.
func ExpectListsFillSum(n int64) int64 { return n * (n - 1) / 2 }
