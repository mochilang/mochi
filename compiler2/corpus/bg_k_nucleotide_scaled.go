package corpus

import "mochi/compiler2/ir"

// BuildKNucleotide is the MEP-39 §6.7 parameterised BG k_nucleotide
// kernel. The canonical BG k_nucleotide program reads the fasta
// output (a DNA byte stream) and counts k-length substrings into a
// hash map, reporting counts for k=1, k=2, and a handful of specific
// k=3..18 mers ("GGT", "GGTA", ...). Iteration 1 of the cross-lang
// harness reduces that to the load-bearing inner kernel: an LCG
// random stream (same parameters as fasta §6.6) drives a HOMO_SAPIENS
// cumprob lookup that returns a 2-bit code (0/1/2/3 for a/c/g/t);
// the kernel maintains 1-mer and 2-mer counts in a single int-keyed
// map, then folds the counts into a deterministic i64 hash so the
// cross-lang harness compares a single integer across every peer.
//
// Key layout in the map:
//
//	keys  0..3   = 1-mer counts indexed by code
//	keys  4..19  = 2-mer counts indexed by `4 + prev*4 + cur`
//
// Iteration 1 ignores k=3..18 (the canonical specific-mer table);
// MEP-39 §6.7 names the iteration 2 mechanism that extends the map
// to k=3 and folds the byte→code conversion through the canonical
// 97/99/103/116 byte alphabet (lookup currently returns the code
// directly to keep iteration 1 focused on the map-increment cost,
// which is the load-bearing measurement for this kernel).
//
// LCG (matches fasta):
//
//	seed = (seed * 3877 + 29573) % 139968    // canonical BG params
//	prob = float64(seed) / 139968.0
//
// HOMO_SAPIENS cumprob → 2-bit code:
//
//	prob < 0.3029549426680  → 0 ('a')
//	prob < 0.5009432431601  → 1 ('c')
//	prob < 0.6984905497992  → 2 ('g')
//	otherwise               → 3 ('t')
//
// Rolling i64 hash (over keys 0..19):
//
//	h = (h * 1009 + count[key]) % 2147483647
//
// Four IR functions:
//
//	main()                    = bootstrap iter 0 inline, call loop, call summ
//	loop(seed, m, prev, i, n) = TUnit tail-rec; one LCG step + 1-mer inc + 2-mer inc
//	summ(m, key, acc, end)    = TI64 tail-rec; walks 0..end, rolls hash
//	lookup(prob) -> TI64      = 4-way FP cascade returning 0..3
//
// The map increments are inlined inside `loop` (no helper call) to
// keep the per-iter dispatch count tight: each iter dispatches one
// MapGet + one AddI64 + one MapSet per k, plus the LCG and the
// lookup call. With CNull().Int() == 0, the missing-key path for
// integer values is "first inc reads 0, writes 1" without any
// MapHas guard — no pre-init pass needed.
func BuildKNucleotide(n int64) *ir.Module {
	const (
		loopIdx   = 1
		summIdx   = 2
		lookupIdx = 3
	)

	// main(): bootstrap iter 0 inline (one LCG step + one 1-mer inc),
	// then drive the rest of the iterations via `loop`, then summarise.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	m := bMain.NewMap()
	// LCG step 0: s0 = (42*3877 + 29573) % 139968
	seed0 := bMain.ConstI64(42)
	mul0 := bMain.MulI64(seed0, bMain.ConstI64(3877))
	add0 := bMain.AddI64(mul0, bMain.ConstI64(29573))
	s0 := bMain.ModI64(add0, bMain.ConstI64(139968))
	prob0 := bMain.DivF64(bMain.I64ToF64(s0), bMain.ConstF64(139968.0))
	code0 := bMain.Call(lookupIdx, ir.TI64, prob0)
	// m[code0] += 1   (missing key reads as 0)
	v0 := bMain.MapGet(m, code0, ir.TI64)
	bMain.MapSet(m, code0, bMain.AddI64(v0, bMain.ConstI64(1)))
	// drive remaining N-1 iters via loop, starting at i=1, prev=code0
	bMain.Call(loopIdx, ir.TUnit, s0, m, code0, bMain.ConstI64(1), bMain.ConstI64(n))
	// summarise: walk 0..20, fold counts into rolling i64 hash
	h := bMain.Call(summIdx, ir.TI64, m, bMain.ConstI64(0), bMain.ConstI64(0), bMain.ConstI64(20))
	bMain.Ret(h)

	// loop(seed, m, prev, i, n) -> TUnit tail-rec.
	bL := ir.NewBuilder("loop",
		[]ir.Type{ir.TI64, ir.TMap, ir.TI64, ir.TI64, ir.TI64}, ir.TUnit)
	pSeed, pM, pPrev, pI, pN := bL.Param(0), bL.Param(1), bL.Param(2), bL.Param(3), bL.Param(4)
	lDone := bL.NewBlock()
	lStep := bL.NewBlock()
	bL.CondBr(bL.LessI64(pI, pN), lStep, lDone)
	bL.SwitchTo(lDone)
	bL.Ret(-1)
	bL.SwitchTo(lStep)
	// LCG step
	mul := bL.MulI64(pSeed, bL.ConstI64(3877))
	add := bL.AddI64(mul, bL.ConstI64(29573))
	newSeed := bL.ModI64(add, bL.ConstI64(139968))
	// prob + lookup
	prob := bL.DivF64(bL.I64ToF64(newSeed), bL.ConstF64(139968.0))
	code := bL.Call(lookupIdx, ir.TI64, prob)
	// 1-mer increment: m[code] += 1
	v1 := bL.MapGet(pM, code, ir.TI64)
	bL.MapSet(pM, code, bL.AddI64(v1, bL.ConstI64(1)))
	// 2-mer increment: m[4 + prev*4 + code] += 1
	key2 := bL.AddI64(bL.ConstI64(4), bL.AddI64(bL.MulI64(pPrev, bL.ConstI64(4)), code))
	v2 := bL.MapGet(pM, key2, ir.TI64)
	bL.MapSet(pM, key2, bL.AddI64(v2, bL.ConstI64(1)))
	// recurse with code becoming the new prev
	ni := bL.AddI64(pI, bL.ConstI64(1))
	bL.Call(loopIdx, ir.TUnit, newSeed, pM, code, ni, pN)
	bL.Ret(-1)

	// summ(m, key, acc, end) -> TI64 tail-rec. Walks key..end and
	// folds m[k] into acc via `(acc*1009 + c) % 2147483647`.
	bS := ir.NewBuilder("summ",
		[]ir.Type{ir.TMap, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sM, sK, sAcc, sEnd := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sK, sEnd), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	c := bS.MapGet(sM, sK, ir.TI64)
	newAcc := bS.ModI64(bS.AddI64(bS.MulI64(sAcc, bS.ConstI64(1009)), c), bS.ConstI64(2147483647))
	nk := bS.AddI64(sK, bS.ConstI64(1))
	rec := bS.Call(summIdx, ir.TI64, sM, nk, newAcc, sEnd)
	bS.Ret(rec)

	// lookup(prob TF64) -> TI64 in {0,1,2,3}.
	bLU := ir.NewBuilder("lookup", []ir.Type{ir.TF64}, ir.TI64)
	pP := bLU.Param(0)
	luA := bLU.NewBlock() // return 0 ('a')
	luChkC := bLU.NewBlock()
	luC := bLU.NewBlock() // return 1 ('c')
	luChkG := bLU.NewBlock()
	luG := bLU.NewBlock() // return 2 ('g')
	luT := bLU.NewBlock() // return 3 ('t')
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.3029549426680)), luA, luChkC)
	bLU.SwitchTo(luA)
	bLU.Ret(bLU.ConstI64(0))
	bLU.SwitchTo(luChkC)
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.5009432431601)), luC, luChkG)
	bLU.SwitchTo(luC)
	bLU.Ret(bLU.ConstI64(1))
	bLU.SwitchTo(luChkG)
	bLU.CondBr(bLU.LessF64(pP, bLU.ConstF64(0.6984905497992)), luG, luT)
	bLU.SwitchTo(luG)
	bLU.Ret(bLU.ConstI64(2))
	bLU.SwitchTo(luT)
	bLU.Ret(bLU.ConstI64(3))

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(), bS.Function(), bLU.Function(),
	}, Main: 0}
}

// ExpectKNucleotide runs the same algorithm in plain Go so the oracle
// test can assert vm2 produces the bit-identical i64 hash.
func ExpectKNucleotide(n int64) int64 {
	counts := make(map[int64]int64, 20)
	lookup := func(prob float64) int64 {
		switch {
		case prob < 0.3029549426680:
			return 0
		case prob < 0.5009432431601:
			return 1
		case prob < 0.6984905497992:
			return 2
		default:
			return 3
		}
	}
	// bootstrap iter 0
	seed := (int64(42)*3877 + 29573) % 139968
	prev := lookup(float64(seed) / 139968.0)
	counts[prev]++
	// iters 1..n-1
	for i := int64(1); i < n; i++ {
		seed = (seed*3877 + 29573) % 139968
		code := lookup(float64(seed) / 139968.0)
		counts[code]++
		counts[4+prev*4+code]++
		prev = code
	}
	// summarise
	var h int64
	for k := int64(0); k < 20; k++ {
		h = (h*1009 + counts[k]) % 2147483647
	}
	return h
}
