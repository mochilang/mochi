package corpus

import "mochi/compiler2/ir"

// BuildMapsFillSum builds the map analogue of BuildListsFillSum:
//
//	m := {}
//	for i := 0; i < N; i++ { m[i] = i }
//	s := 0
//	for j := 0; j < N; j++ { s += m[j] }
//	return s
//
// Same two self-tail-call helpers (fill, sum) so each loop folds to
// OpTailCallSelf. The benchmark exercises OpNewMap + OpMapSet +
// OpMapGet at MVP using int keys, where Go's map[any]Cell stores int64
// keys directly.
func BuildMapsFillSum(n int64) *ir.Module {
	const fillIdx = 1
	const sumIdx = 2

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	m := bMain.NewMap()
	i0 := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	bMain.Call(fillIdx, ir.TUnit, m, i0, nv)
	s0 := bMain.ConstI64(0)
	j0 := bMain.ConstI64(0)
	r := bMain.Call(sumIdx, ir.TI64, m, j0, nv, s0)
	bMain.Ret(r)

	bF := ir.NewBuilder("fill", []ir.Type{ir.TMap, ir.TI64, ir.TI64}, ir.TUnit)
	pM, pI, pN := bF.Param(0), bF.Param(1), bF.Param(2)
	done := bF.NewBlock()
	step := bF.NewBlock()
	bF.CondBr(bF.LessI64(pI, pN), step, done)
	bF.SwitchTo(done)
	bF.Ret(-1)
	bF.SwitchTo(step)
	bF.MapSet(pM, pI, pI)
	one := bF.ConstI64(1)
	ni := bF.AddI64(pI, one)
	bF.Call(fillIdx, ir.TUnit, pM, ni, pN)
	bF.Ret(-1)

	bS := ir.NewBuilder("sum", []ir.Type{ir.TMap, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sM, sJ, sN, sAcc := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sJ, sN), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	x := bS.MapGet(sM, sJ, ir.TI64)
	nAcc := bS.AddI64(sAcc, x)
	sOne := bS.ConstI64(1)
	nJ := bS.AddI64(sJ, sOne)
	r2 := bS.Call(sumIdx, ir.TI64, sM, nJ, sN, nAcc)
	bS.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bF.Function(), bS.Function()}, Main: 0}
}

// ExpectMapsFillSum returns the expected sum, identical to fill_sum:
// sum 0..n-1 == n*(n-1)/2.
func ExpectMapsFillSum(n int64) int64 { return n * (n - 1) / 2 }
