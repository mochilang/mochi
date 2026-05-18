package corpus

import "mochi/compiler2/ir"

// BuildFasta is the MEP-39 §6.6 parameterised BG fasta kernel. The
// canonical BG fasta program emits three FASTA records (an ALU header
// followed by IUB and HOMO_SAPIENS random sequences). Iteration 1 of
// the cross-lang harness reduces that to the load-bearing inner
// kernel: an LCG random generator paired with a cumulative-probability
// table lookup, summarised by a Horner-style hash so the cross-lang
// harness compares a single i64 across every peer.
//
// Iteration 1 ran the HOMO_SAPIENS 4-entry table through an FP cascade
// invoked via a Call to a separate `lookup` function: every iter cost
// one I64ToF64 + DivF64 to build prob = seed/139968.0, one Call to
// lookup (with full frame setup), an average of two LessF64 comparisons
// inside lookup, and a Return. Iteration 2 (this version) inlines the
// cascade into the loop body and uses precomputed i64 thresholds. The
// loop fans out four tail-recursive leaves (one per a/c/g/t bucket);
// opt.TailCall still fuses each leaf's Ret(call_self) into a single
// OpTailCallSelfA4 dispatch, so the inlining costs no extra control-
// flow ops. Net per-iter dispatch drops from ~18 (iter 1) to ~13
// (iter 2). The thresholds are picked so the i64 comparison is
// bit-identical to the original FP cascade for every seed in
// [0, 139968). See MEP-39 §6.6 mechanism 2 for the lift theory.
//
// LCG:
//
//	seed = (seed * 3877 + 29573) % 139968    // Lehmer/canonical BG params
//
// HOMO_SAPIENS cumprob table, post-iteration-2 (i64 thresholds; each is
// the smallest seed s where float64(s)/139968.0 fails the FP compare,
// computed once in computeFastaThresholds below):
//
//	seed <  42404 → 'a' (97)     // FP equiv: prob < 0.3029549426680
//	seed <  70117 → 'c' (99)     // FP equiv: prob < 0.5009432431601
//	seed <  97767 → 'g' (103)    // FP equiv: prob < 0.6984905497992
//	otherwise     → 't' (116)
//
// Rolling i64 hash:
//
//	hash = (hash * 1009 + byte) % 2147483647    // Mersenne prime mod
//
// Two IR functions:
//
//	main()                    = loop(42, 0, 0, N); return hash
//	loop(seed, hash, i, n)    = tail-rec, LCG step + inline lookup
//	                            cascade + hash update; 4 tail-call leaves
//
// Each leaf ends with `Ret(call_self)` so opt.TailCall folds them all
// into OpTailCallSelfA4 dispatches.
func BuildFasta(n int64) *ir.Module {
	const loopIdx = 1
	thrA, thrC, thrG := fastaThrA, fastaThrC, fastaThrG

	// main(): call loop(42, 0, 0, N), return its result.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	seed0 := bMain.ConstI64(42)
	hash0 := bMain.ConstI64(0)
	i0 := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	r := bMain.Call(loopIdx, ir.TI64, seed0, hash0, i0, nv)
	bMain.Ret(r)

	// loop(seed, hash, i, n):
	bL := ir.NewBuilder("loop",
		[]ir.Type{ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	pSeed, pHash, pI, pN := bL.Param(0), bL.Param(1), bL.Param(2), bL.Param(3)
	lDone := bL.NewBlock()
	lStep := bL.NewBlock()
	bChkC := bL.NewBlock()
	bChkG := bL.NewBlock()
	bA := bL.NewBlock()
	bC := bL.NewBlock()
	bG := bL.NewBlock()
	bT := bL.NewBlock()

	bL.CondBr(bL.LessI64(pI, pN), lStep, lDone)
	bL.SwitchTo(lDone)
	bL.Ret(pHash)

	bL.SwitchTo(lStep)
	// LCG: new_seed = (seed * 3877 + 29573) % 139968
	mul := bL.MulI64(pSeed, bL.ConstI64(3877))
	add := bL.AddI64(mul, bL.ConstI64(29573))
	newSeed := bL.ModI64(add, bL.ConstI64(139968))
	// Inline lookup cascade against precomputed i64 thresholds.
	bL.CondBr(bL.LessI64(newSeed, bL.ConstI64(thrA)), bA, bChkC)
	bL.SwitchTo(bChkC)
	bL.CondBr(bL.LessI64(newSeed, bL.ConstI64(thrC)), bC, bChkG)
	bL.SwitchTo(bChkG)
	bL.CondBr(bL.LessI64(newSeed, bL.ConstI64(thrG)), bG, bT)

	emitLeaf := func(block ir.BlockID, byteVal int64) {
		bL.SwitchTo(block)
		hMul := bL.MulI64(pHash, bL.ConstI64(1009))
		hAdd := bL.AddI64(hMul, bL.ConstI64(byteVal))
		newHash := bL.ModI64(hAdd, bL.ConstI64(2147483647))
		nextI := bL.AddI64(pI, bL.ConstI64(1))
		rec := bL.Call(loopIdx, ir.TI64, newSeed, newHash, nextI, pN)
		bL.Ret(rec)
	}
	emitLeaf(bA, 97)
	emitLeaf(bC, 99)
	emitLeaf(bG, 103)
	emitLeaf(bT, 116)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(),
	}, Main: 0}
}

// fastaThrA / fastaThrC / fastaThrG are the i64 cutoffs used by the
// iteration-2 lookup. They are computed once at package init via the
// same FP arithmetic the iteration-1 builder used, so the i64 path is
// bit-identical to the FP cascade (same byte, same hash) for every
// seed in [0, 139968). See ExpectFasta for the matching oracle.
var (
	fastaThrA int64
	fastaThrC int64
	fastaThrG int64
)

func init() {
	computeFastaThresholds()
}

func computeFastaThresholds() {
	for s := int64(0); s < 139968; s++ {
		p := float64(s) / 139968.0
		if p < 0.3029549426680 {
			fastaThrA = s + 1
		}
		if p < 0.5009432431601 {
			fastaThrC = s + 1
		}
		if p < 0.6984905497992 {
			fastaThrG = s + 1
		}
	}
}

// ExpectFasta runs the same algorithm in plain Go so the oracle test
// can assert vm2 produces the bit-identical int64 hash.
func ExpectFasta(n int64) int64 {
	seed := int64(42)
	hash := int64(0)
	for i := int64(0); i < n; i++ {
		seed = (seed*3877 + 29573) % 139968
		prob := float64(seed) / 139968.0
		var b int64
		switch {
		case prob < 0.3029549426680:
			b = 97
		case prob < 0.5009432431601:
			b = 99
		case prob < 0.6984905497992:
			b = 103
		default:
			b = 116
		}
		hash = (hash*1009 + b) % 2147483647
	}
	return hash
}
