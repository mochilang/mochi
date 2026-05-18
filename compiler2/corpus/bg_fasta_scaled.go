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
// Iteration 1 uses the HOMO_SAPIENS 4-entry table (a, c, g, t). The
// 15-entry IUB table is iteration 2; the integer-threshold optimisation
// (replace FP comparisons with precomputed i64 thresholds) is iteration
// 3. See MEP-39 §6.6 for the mechanism ranking.
//
// LCG:
//
//	seed = (seed * 3877 + 29573) % 139968    // Lehmer/canonical BG params
//	prob = float64(seed) / 139968.0
//
// HOMO_SAPIENS cumprob table (cumulative):
//
//	0.3029549426680 → 'a' (97)
//	0.5009432431601 → 'c' (99)
//	0.6984905497992 → 'g' (103)
//	1.0             → 't' (116)
//
// Rolling i64 hash:
//
//	hash = (hash * 1009 + byte) % 2147483647    // Mersenne prime mod
//
// Three IR functions:
//
//	main()                    = loop(42, 0, 0, N); return hash
//	loop(seed, hash, i, n)    = tail-rec, one LCG step + one lookup + one hash update
//	lookup(prob) -> byte      = 4-way FP cascade
//
// `loop` returns TI64 (the hash) via Ret(call_result) so opt.TailCall
// folds it into a single OpTailCallSelfA4. `lookup` is TI64 and uses
// straight returns from leaf blocks.
func BuildFasta(n int64) *ir.Module {
	const (
		loopIdx   = 1
		lookupIdx = 2
	)

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
	bL.CondBr(bL.LessI64(pI, pN), lStep, lDone)
	bL.SwitchTo(lDone)
	bL.Ret(pHash)
	bL.SwitchTo(lStep)
	// LCG: new_seed = (seed * 3877 + 29573) % 139968
	mul := bL.MulI64(pSeed, bL.ConstI64(3877))
	add := bL.AddI64(mul, bL.ConstI64(29573))
	newSeed := bL.ModI64(add, bL.ConstI64(139968))
	// prob = float64(new_seed) / 139968.0
	seedF := bL.I64ToF64(newSeed)
	prob := bL.DivF64(seedF, bL.ConstF64(139968.0))
	// byte = lookup(prob)
	byteV := bL.Call(lookupIdx, ir.TI64, prob)
	// hash update: new_hash = (hash * 1009 + byte) % 2147483647
	hMul := bL.MulI64(pHash, bL.ConstI64(1009))
	hAdd := bL.AddI64(hMul, byteV)
	newHash := bL.ModI64(hAdd, bL.ConstI64(2147483647))
	// recurse
	rec := bL.Call(loopIdx, ir.TI64,
		newSeed, newHash, bL.AddI64(pI, bL.ConstI64(1)), pN)
	bL.Ret(rec)

	// lookup(prob): 4-way FP cascade returning the byte for that bucket.
	bLU := ir.NewBuilder("lookup", []ir.Type{ir.TF64}, ir.TI64)
	pP := bLU.Param(0)
	luAB := bLU.NewBlock() // return 'a'
	luChkC := bLU.NewBlock()
	luCB := bLU.NewBlock() // return 'c'
	luChkG := bLU.NewBlock()
	luGB := bLU.NewBlock() // return 'g'
	luTB := bLU.NewBlock() // return 't'
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.3029549426680)), luAB, luChkC)
	bLU.SwitchTo(luAB)
	bLU.Ret(bLU.ConstI64(97))
	bLU.SwitchTo(luChkC)
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.5009432431601)), luCB, luChkG)
	bLU.SwitchTo(luCB)
	bLU.Ret(bLU.ConstI64(99))
	bLU.SwitchTo(luChkG)
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.6984905497992)), luGB, luTB)
	bLU.SwitchTo(luGB)
	bLU.Ret(bLU.ConstI64(103))
	bLU.SwitchTo(luTB)
	bLU.Ret(bLU.ConstI64(116))

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(), bLU.Function(),
	}, Main: 0}
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
