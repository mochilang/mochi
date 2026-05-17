package corpus

import "mochi/compiler2/ir"

// BuildBinaryTrees builds the Computer Language Benchmarks Game
// `binary-trees` kernel: build 2^N short-lived trees of depth N and sum
// their node counts. Each tree allocation grows and is then unreferenced,
// so the program is dominated by container-construction throughput and
// the runtime's ability to reclaim dead structures. This is the headline
// benchmark for [MEP 36]'s container-GC contract.
//
// vm2's IR has no struct support yet, so trees are encoded as nested
// lists: a leaf is the empty list `[]`, and an internal node is a
// 2-element list `[left, right]`. Both leaves and internal nodes flow
// through the same OpListGet / OpListLen path. This is faithful to the
// BG specification (each subtree is its own allocation) and exercises
// the exact code path MEP 36 cares about: many small list allocations
// dying as their referring frame returns.
//
// The number of outer iterations is constant-folded at build time
// because N is known when the IR is constructed; we precompute
// `iters = 1 << N` rather than emitting a separate power-of-two helper.
//
//	main()                     = outer(N, 0, iters, 0)
//	make_tree(depth)           = if depth==0 then [] else [make_tree(d-1), make_tree(d-1)]
//	check_tree(t)              = if len(t)==0 then 1 else 1 + check_tree(t[0]) + check_tree(t[1])
//	outer(d, i, iters, acc)    = if i>=iters then acc else recurse(d, i+1, iters, acc + check_tree(make_tree(d)))
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

	bM := ir.NewBuilder("make_tree", []ir.Type{ir.TI64}, ir.TList)
	pD := bM.Param(0)
	leafB := bM.NewBlock()
	nodeB := bM.NewBlock()
	zeroM := bM.ConstI64(0)
	isLeaf := bM.EqualI64(pD, zeroM)
	bM.CondBr(isLeaf, leafB, nodeB)

	bM.SwitchTo(leafB)
	leaf := bM.NewList(0)
	bM.Ret(leaf)

	bM.SwitchTo(nodeB)
	one := bM.ConstI64(1)
	dm1 := bM.SubI64(pD, one)
	left := bM.Call(makeIdx, ir.TList, dm1)
	right := bM.Call(makeIdx, ir.TList, dm1)
	node := bM.NewList(2)
	bM.ListPush(node, left)
	bM.ListPush(node, right)
	bM.Ret(node)

	bC := ir.NewBuilder("check_tree", []ir.Type{ir.TList}, ir.TI64)
	pT := bC.Param(0)
	cLeafB := bC.NewBlock()
	cNodeB := bC.NewBlock()
	lenT := bC.ListLen(pT)
	zeroC := bC.ConstI64(0)
	isEmpty := bC.EqualI64(lenT, zeroC)
	bC.CondBr(isEmpty, cLeafB, cNodeB)

	bC.SwitchTo(cLeafB)
	oneC := bC.ConstI64(1)
	bC.Ret(oneC)

	bC.SwitchTo(cNodeB)
	idx0 := bC.ConstI64(0)
	idx1 := bC.ConstI64(1)
	leftT := bC.ListGet(pT, idx0, ir.TList)
	rightT := bC.ListGet(pT, idx1, ir.TList)
	lc := bC.Call(checkIdx, ir.TI64, leftT)
	rc := bC.Call(checkIdx, ir.TI64, rightT)
	sum := bC.AddI64(lc, rc)
	oneN := bC.ConstI64(1)
	rr := bC.AddI64(sum, oneN)
	bC.Ret(rr)

	bO := ir.NewBuilder("outer", []ir.Type{ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	oD, oI, oIters, oAcc := bO.Param(0), bO.Param(1), bO.Param(2), bO.Param(3)
	oDone := bO.NewBlock()
	oStep := bO.NewBlock()
	bO.CondBr(bO.LessI64(oI, oIters), oStep, oDone)
	bO.SwitchTo(oDone)
	bO.Ret(oAcc)
	bO.SwitchTo(oStep)
	tree := bO.Call(makeIdx, ir.TList, oD)
	cVal := bO.Call(checkIdx, ir.TI64, tree)
	nAcc := bO.AddI64(oAcc, cVal)
	oneO := bO.ConstI64(1)
	ni := bO.AddI64(oI, oneO)
	r2 := bO.Call(outerIdx, ir.TI64, oD, ni, oIters, nAcc)
	bO.Ret(r2)

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
