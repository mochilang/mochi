package corpus

import "mochi/compiler2/ir"

// BuildStringsConcatLoop builds:
//
//	acc := "a"
//	for i := 0; i < N; i++ { acc = acc ++ "a" }
//	return len(acc)
//
// Encoded as a tail-recursive helper loop(acc, lit, i, n) so the
// emitter folds it into a self-tail-call. Returns the byte length of
// the final string (== n+1) so the runner can assert a single scalar.
func BuildStringsConcatLoop(n int64) *ir.Module {
	const helperIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	acc0 := bMain.ConstStr("a")
	lit := bMain.ConstStr("a")
	i0 := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	r := bMain.Call(helperIdx, ir.TI64, acc0, lit, i0, nv)
	bMain.Ret(r)

	bH := ir.NewBuilder("concat_loop", []ir.Type{ir.TStr, ir.TStr, ir.TI64, ir.TI64}, ir.TI64)
	pAcc, pLit, pI, pN := bH.Param(0), bH.Param(1), bH.Param(2), bH.Param(3)
	done := bH.NewBlock()
	step := bH.NewBlock()
	cond := bH.LessI64(pI, pN)
	bH.CondBr(cond, step, done)

	bH.SwitchTo(done)
	bH.Ret(bH.LenStr(pAcc))

	bH.SwitchTo(step)
	one := bH.ConstI64(1)
	ni := bH.AddI64(pI, one)
	nacc := bH.ConcatStr(pAcc, pLit)
	r2 := bH.Call(helperIdx, ir.TI64, nacc, pLit, ni, pN)
	bH.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bH.Function()}, Main: 0}
}

// ExpectStringsConcatLoop: starts at length 1 ("a") then n appends.
func ExpectStringsConcatLoop(n int64) int64 { return n + 1 }
