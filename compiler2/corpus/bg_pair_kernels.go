package corpus

import "mochi/compiler2/ir"

// BuildPairSwapKernel builds a tail-recursive pair-swap loop. Each
// iteration reads the two halves of the current pair and constructs a
// fresh pair with the slots swapped. The chain head escapes through the
// final return so the swap can't be elided. MEP-38 §A.5: this is the
// minimum-body pair workload, where every loop step does exactly one
// arena bump and two projection reads. Go must heap-allocate each
// swapped pair; vm2 bumps the pair arena.
//
// Shape:
//
//	main()              = loop(0, N, pair(0, 1))
//	loop(i, n, head)    = if i >= n then head
//	                      else loop(i+1, n, pair(head.snd, head.fst))
//
// Steady-state body (post-fusion): OpJumpIfLessI64, OpPairFst, OpPairSnd,
// OpNewPair, OpTailCallSelf, OpAddI64K. The PairFst/PairSnd here cannot
// fuse into the OpCallA2 form because both halves are consumed by the
// NewPair, not the call.
func BuildPairSwapKernel(n int64) *ir.Module {
	const loopIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TPair)
	zero := bMain.ConstI64(0)
	one := bMain.ConstI64(1)
	nv := bMain.ConstI64(n)
	seed := bMain.NewPair(zero, one)
	r := bMain.Call(loopIdx, ir.TPair, zero, nv, seed)
	bMain.Ret(r)

	bL := ir.NewBuilder("loop",
		[]ir.Type{ir.TI64, ir.TI64, ir.TPair}, ir.TPair)
	i, nParam, head := bL.Param(0), bL.Param(1), bL.Param(2)
	done := bL.NewBlock()
	step := bL.NewBlock()
	bL.CondBr(bL.LessI64(i, nParam), step, done)
	bL.SwitchTo(done)
	bL.Ret(head)
	bL.SwitchTo(step)
	k1 := bL.ConstI64(1)
	i1 := bL.AddI64(i, k1)
	fst := bL.PairFst(head, ir.TI64)
	snd := bL.PairSnd(head, ir.TI64)
	next := bL.NewPair(snd, fst)
	r2 := bL.Call(loopIdx, ir.TPair, i1, nParam, next)
	bL.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(),
	}, Main: 0}
}

// BuildPairWalkKernel builds a chain of N pairs via a tail-recursive
// build phase, then walks the chain in a second tail-recursive count
// phase, returning the chain length as int64. The build phase exposes
// arena throughput; the walk phase exposes the cost of pair-projection
// access patterns vs Go's pointer chase.
//
// Shape:
//
//	main()           = walk(build(0, N, pair(0, 0)), 0)
//	build(i, n, h)   = if i >= n then h
//	                   else build(i+1, n, pair(0, h))
//	walk(h, acc)     = if isLeaf(h) then acc
//	                   else walk(h.snd, acc+1)
//
// The leaf test uses i64 sentinel pairs: a "leaf" is the seed pair
// pair(0, 0) where snd's projected value equals itself (snd == leaf
// would require pointer compare we don't expose). Instead we count via
// the build/walk pass-through length parameter and use the natural
// chain length as the result; the walk runs exactly N steps unguarded.
func BuildPairWalkKernel(n int64) *ir.Module {
	const (
		buildIdx = 1
		walkIdx  = 2
	)

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	zero := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	seed := bMain.NewPair(zero, zero)
	chain := bMain.Call(buildIdx, ir.TPair, zero, nv, seed)
	count := bMain.Call(walkIdx, ir.TI64, chain, zero, nv)
	bMain.Ret(count)

	// build(i, n, head) -> TPair: i++ steps allocating pair(0, head).
	bB := ir.NewBuilder("build",
		[]ir.Type{ir.TI64, ir.TI64, ir.TPair}, ir.TPair)
	bi, bn, bh := bB.Param(0), bB.Param(1), bB.Param(2)
	bDone := bB.NewBlock()
	bStep := bB.NewBlock()
	bB.CondBr(bB.LessI64(bi, bn), bStep, bDone)
	bB.SwitchTo(bDone)
	bB.Ret(bh)
	bB.SwitchTo(bStep)
	k1b := bB.ConstI64(1)
	bi1 := bB.AddI64(bi, k1b)
	zero2 := bB.ConstI64(0)
	bNext := bB.NewPair(zero2, bh)
	br := bB.Call(buildIdx, ir.TPair, bi1, bn, bNext)
	bB.Ret(br)

	// walk(head, acc, n) -> TI64: chases head.snd N times, incrementing acc.
	bW := ir.NewBuilder("walk",
		[]ir.Type{ir.TPair, ir.TI64, ir.TI64}, ir.TI64)
	wh, wa, wn := bW.Param(0), bW.Param(1), bW.Param(2)
	wDone := bW.NewBlock()
	wStep := bW.NewBlock()
	bW.CondBr(bW.LessI64(wa, wn), wStep, wDone)
	bW.SwitchTo(wDone)
	bW.Ret(wa)
	bW.SwitchTo(wStep)
	wnext := bW.PairSnd(wh, ir.TPair)
	k1w := bW.ConstI64(1)
	wa1 := bW.AddI64(wa, k1w)
	wr := bW.Call(walkIdx, ir.TI64, wnext, wa1, wn)
	bW.Ret(wr)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bB.Function(), bW.Function(),
	}, Main: 0}
}
