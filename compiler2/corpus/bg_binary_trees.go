package corpus

import "mochi/compiler2/ir"

// BuildBinaryTreesKernel builds the BG binary_trees kernel using
// MEP-37 §3.4 packed pairs as node storage. For a given depth d the
// kernel builds a perfect binary tree of that depth (2^(d+1) - 1
// nodes) and walks it counting each node, returning 2^(d+1) - 1.
//
// Three hand-written IR functions:
//
//	main()              = checkTree(makeTree(d), d)
//	makeTree(d) -> TPair = pair(makeTree(d-1), makeTree(d-1)) at depth d>0;
//	                       pair(0, 0) leaf at d=0.
//	checkTree(t, d) -> i64 = 1 at d=0; 1 + check(fst,d-1) + check(snd,d-1).
//
// Neither helper tail-recurses (both recursive calls' results combine
// into the parent's result through AddI64), so the kernel routes
// through OpCall throughout. The per-call overhead is what binary_trees
// stresses — the value of MEP-37 §3.4's pair arena is that the 2^(d+1)
// node allocations become 2^(d+1)/256 chunk allocations + index
// bumps, not 2^(d+1) Go heap allocations.
//
// Tree leaves carry int(0) in both slots so a leaf pair has the same
// shape as a branch pair. checkTree distinguishes them by the depth
// parameter, not by inspecting the cell, which means the IR
// constructor lays out leaves and branches identically.
func BuildBinaryTreesKernel(depth int64) *ir.Module {
	const (
		makeIdx  = 1
		checkIdx = 2
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	dv := bMain.ConstI64(depth)
	tree := bMain.Call(makeIdx, ir.TPair, dv)
	sum := bMain.Call(checkIdx, ir.TI64, tree, dv)
	bMain.Ret(sum)

	// makeTree(d): if d==0 return pair(0,0); else pair(makeTree(d-1), makeTree(d-1)).
	bM := ir.NewBuilder("makeTree", []ir.Type{ir.TI64}, ir.TPair)
	md := bM.Param(0)
	mLeaf := bM.NewBlock()
	mBranch := bM.NewBlock()
	bM.CondBr(bM.EqualI64(md, bM.ConstI64(0)), mLeaf, mBranch)
	bM.SwitchTo(mLeaf)
	bM.Ret(bM.NewPair(bM.ConstI64(0), bM.ConstI64(0)))
	bM.SwitchTo(mBranch)
	dm1 := bM.SubI64(md, bM.ConstI64(1))
	l := bM.Call(makeIdx, ir.TPair, dm1)
	r := bM.Call(makeIdx, ir.TPair, dm1)
	bM.Ret(bM.NewPair(l, r))

	// checkTree(t, d): if d==0 return 1; else 1 + check(fst, d-1) + check(snd, d-1).
	bC := ir.NewBuilder("checkTree",
		[]ir.Type{ir.TPair, ir.TI64}, ir.TI64)
	ct, cd := bC.Param(0), bC.Param(1)
	cLeaf := bC.NewBlock()
	cBranch := bC.NewBlock()
	bC.CondBr(bC.EqualI64(cd, bC.ConstI64(0)), cLeaf, cBranch)
	bC.SwitchTo(cLeaf)
	bC.Ret(bC.ConstI64(1))
	bC.SwitchTo(cBranch)
	dm1c := bC.SubI64(cd, bC.ConstI64(1))
	lc := bC.PairFst(ct, ir.TPair)
	rc := bC.PairSnd(ct, ir.TPair)
	lcnt := bC.Call(checkIdx, ir.TI64, lc, dm1c)
	rcnt := bC.Call(checkIdx, ir.TI64, rc, dm1c)
	s := bC.AddI64(bC.AddI64(bC.ConstI64(1), lcnt), rcnt)
	bC.Ret(s)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bM.Function(), bC.Function(),
	}, Main: 0}
}
