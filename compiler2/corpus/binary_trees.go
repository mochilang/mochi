package corpus

import "mochi/compiler2/ir"

// BuildBinaryTrees builds the Computer Language Benchmarks Game
// `binary-trees` kernel: build 2^N short-lived trees of depth N and
// sum their node counts. Each tree allocation grows and is then
// unreferenced, so the program is dominated by container-construction
// throughput and the runtime's ability to reclaim dead structures.
//
// MEP-39 §6.10 iteration 2 switched the per-node storage from nested
// 2-element lists (`OpNewList(2)` + 2 × `OpListPush`) to MEP-37 §3.4
// packed pairs (one `OpNewPair`). Per node that's 1 dispatch instead of
// 3, and the pair arena bumps a cursor in a 256-slot chunk so a depth-N
// tree allocates 2^(N+1)/256 chunks instead of 2^(N+1) individual Go
// list+slice pairs. The leaf-vs-branch test moves to a depth-based
// shape (pairs are indistinguishable structurally), matching the
// canonical BG reference (which uses `if depth == 0` not `if len == 0`).
//
// The number of outer iterations is constant-folded at build time
// because N is known when the IR is constructed; we precompute
// `iters = 1 << N` rather than emitting a separate power-of-two helper.
//
//	main()                     = outer(N, 0, iters, 0)
//	make_tree(d)               = if d==0 then pair(0,0) else pair(make_tree(d-1), make_tree(d-1))
//	check_tree(t, d)           = if d==0 then 1 else 1 + check_tree(fst, d-1) + check_tree(snd, d-1)
//	outer(d, i, iters, acc)    = if i>=iters then acc else outer(d, i+1, iters, acc + check_tree(make_tree(d), d))
//
// The outer loop is the only tail-recursive helper; check_tree's two
// recursive calls are deliberately non-tail to keep the canonical BG
// recursion shape (the inliner could collapse a single arm, but the
// program's whole point is to recurse).
func BuildBinaryTrees(n int64) *ir.Module {
	const (
		makeIdx  = 1
		checkIdx = 2
		outerIdx = 3
	)

	iters := int64(1)
	for k := int64(0); k < n; k++ {
		iters *= 2
	}

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	depthV := bMain.ConstI64(n)
	zero := bMain.ConstI64(0)
	itersV := bMain.ConstI64(iters)
	acc0 := bMain.ConstI64(0)
	r := bMain.Call(outerIdx, ir.TI64, depthV, zero, itersV, acc0)
	bMain.Ret(r)

	bM := ir.NewBuilder("make_tree", []ir.Type{ir.TI64}, ir.TPair)
	pD := bM.Param(0)
	leafB := bM.NewBlock()
	nodeB := bM.NewBlock()
	bM.CondBr(bM.EqualI64(pD, bM.ConstI64(0)), leafB, nodeB)

	bM.SwitchTo(leafB)
	bM.Ret(bM.NewPair(bM.ConstI64(0), bM.ConstI64(0)))

	bM.SwitchTo(nodeB)
	dm1 := bM.SubI64(pD, bM.ConstI64(1))
	left := bM.Call(makeIdx, ir.TPair, dm1)
	right := bM.Call(makeIdx, ir.TPair, dm1)
	bM.Ret(bM.NewPair(left, right))

	bC := ir.NewBuilder("check_tree", []ir.Type{ir.TPair, ir.TI64}, ir.TI64)
	pT := bC.Param(0)
	pCD := bC.Param(1)
	cLeafB := bC.NewBlock()
	cNodeB := bC.NewBlock()
	bC.CondBr(bC.EqualI64(pCD, bC.ConstI64(0)), cLeafB, cNodeB)

	bC.SwitchTo(cLeafB)
	bC.Ret(bC.ConstI64(1))

	bC.SwitchTo(cNodeB)
	dm1c := bC.SubI64(pCD, bC.ConstI64(1))
	lc := bC.PairFst(pT, ir.TPair)
	rc := bC.PairSnd(pT, ir.TPair)
	lcnt := bC.Call(checkIdx, ir.TI64, lc, dm1c)
	rcnt := bC.Call(checkIdx, ir.TI64, rc, dm1c)
	sum := bC.AddI64(bC.AddI64(bC.ConstI64(1), lcnt), rcnt)
	bC.Ret(sum)

	bO := ir.NewBuilder("outer", []ir.Type{ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	oD, oI, oIters, oAcc := bO.Param(0), bO.Param(1), bO.Param(2), bO.Param(3)
	oDone := bO.NewBlock()
	oStep := bO.NewBlock()
	bO.CondBr(bO.LessI64(oI, oIters), oStep, oDone)

	bO.SwitchTo(oDone)
	bO.Ret(oAcc)

	bO.SwitchTo(oStep)
	tree := bO.Call(makeIdx, ir.TPair, oD)
	cVal := bO.Call(checkIdx, ir.TI64, tree, oD)
	nAcc := bO.AddI64(oAcc, cVal)
	ni := bO.AddI64(oI, bO.ConstI64(1))
	rRec := bO.Call(outerIdx, ir.TI64, oD, ni, oIters, nAcc)
	bO.Ret(rRec)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bM.Function(), bC.Function(), bO.Function(),
	}, Main: 0}
}

// ExpectBinaryTrees: each depth-N tree has 2^(N+1)-1 nodes, and main
// builds 2^N of them, so the sum is iters * (2*iters - 1).
func ExpectBinaryTrees(n int64) int64 {
	iters := int64(1)
	for k := int64(0); k < n; k++ {
		iters *= 2
	}
	perTree := int64(2)*iters - 1 // 2^(n+1) - 1 nodes per tree
	return iters * perTree
}
