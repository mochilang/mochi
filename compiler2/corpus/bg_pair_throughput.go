package corpus

import "mochi/compiler2/ir"

// BuildPairThroughputKernel builds a pair-allocation throughput kernel
// that exposes the structural advantage of MEP-37 §3.4's per-VM pair
// arena over Go's GC heap. The kernel iterates N times, on each step
// allocating a fresh pair whose two slots both reference the previous
// chain head. This forces every allocation to escape (each pair is
// reachable from the result), so neither Go nor vm2 can elide the
// allocation, but the cost model diverges sharply:
//
//	Go:  N pointer-chasing heap allocations + GC trace work to mark the
//	     chain at the next collection cycle.
//	vm2: N pair-arena bump steps (no GC, no header), reused across
//	     vm.Reset() so the steady-state bench has zero growth.
//
// The companion BenchmarkGo_BG_PairThroughputKernel uses the same shape
// (struct{ L, R *pairTree }) for a head-to-head comparison. The kernel
// is the smallest workload that pins the comparison to the pair
// constructor cost: no walks, no math beyond the loop counter, no
// branches per pair.
//
// Shape:
//
//	main()              = loop(0, N, pair(0, 0))
//	loop(i, n, head)    = if i >= n then head
//	                      else loop(i+1, n, pair(head, head))
//
// The loop is tail-recursive in its only recursive call so emit lowers
// it to OpTailCallSelf; the steady-state body is OpJumpIfGreaterEqI64,
// OpNewPair, OpAddI64K, plus parallel-move setup for the tail call.
func BuildPairThroughputKernel(n int64) *ir.Module {
	const loopIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TPair)
	zero := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	seed := bMain.NewPair(zero, zero)
	r := bMain.Call(loopIdx, ir.TPair, zero, nv, seed)
	bMain.Ret(r)

	// loop(i, n, head) -> TPair
	bL := ir.NewBuilder("loop",
		[]ir.Type{ir.TI64, ir.TI64, ir.TPair}, ir.TPair)
	i, nParam, head := bL.Param(0), bL.Param(1), bL.Param(2)
	done := bL.NewBlock()
	step := bL.NewBlock()
	bL.CondBr(bL.LessI64(i, nParam), step, done)
	bL.SwitchTo(done)
	bL.Ret(head)
	bL.SwitchTo(step)
	one := bL.ConstI64(1)
	i1 := bL.AddI64(i, one)
	next := bL.NewPair(head, head)
	r2 := bL.Call(loopIdx, ir.TPair, i1, nParam, next)
	bL.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(),
	}, Main: 0}
}
