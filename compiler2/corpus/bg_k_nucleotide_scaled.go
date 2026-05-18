package corpus

import "mochi/compiler2/ir"

// BuildKNucleotide is the MEP-39 §6.7 parameterised BG k_nucleotide
// kernel. The canonical BG k_nucleotide program reads the fasta
// output (a DNA byte stream) and counts k-length substrings into a
// hash map, reporting counts for k=1, k=2, and a handful of specific
// k=3..18 mers ("GGT", "GGTA", ...). The cross-lang harness reduces
// that to the load-bearing inner kernel: an LCG random stream (same
// parameters as fasta §6.6) drives a HOMO_SAPIENS cumprob lookup
// that returns a 2-bit code (0/1/2/3 for a/c/g/t); the kernel
// maintains 1-mer and 2-mer counts indexed in [0, 20), then folds
// the counts into a deterministic i64 hash so the cross-lang
// harness compares a single integer across every peer.
//
// Slot layout in the counts array (iteration 2):
//
//	idx  0..3   = 1-mer counts indexed by code
//	idx  4..19  = 2-mer counts indexed by `4 + prev*4 + cur`
//
// Iteration 2 (MEP-39 §6.7) collapses the entire per-iter dispatch
// chain (LCG + cumprob lookup + two MapGet/Set pairs) into a single
// super-op OpKNucleotideRun, and swaps the rolling map for a 20-slot
// TI64Array. Both changes are spec-named: §6.7 mechanism 3 (bounded
// i64 array) eliminates the map probe cost, and the bulk super-op
// drops the per-iter dispatch count from ~12 to 1, which is the
// shape iter 7 used to bring reverse_complement under gate.
//
// Rolling i64 hash (over indices 0..19):
//
//	h = (h * 1009 + counts[idx]) % 2147483647
//
// Two IR functions:
//
//	main()                    = allocate counts, KNucleotideRun(counts,n), call summ
//	summ(arr, key, acc, end)  = TI64 tail-rec; walks 0..end, rolls hash
func BuildKNucleotide(n int64) *ir.Module {
	const summIdx = 1

	// main(): allocate a 20-slot i64 counts array, drive the entire
	// k_nucleotide kernel via one super-op, then fold into a hash.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	counts := bMain.NewI64Array(bMain.ConstI64(20))
	bMain.KNucleotideRun(counts, bMain.ConstI64(n))
	h := bMain.Call(summIdx, ir.TI64, counts, bMain.ConstI64(0), bMain.ConstI64(0), bMain.ConstI64(20))
	bMain.Ret(h)

	// summ(arr, key, acc, end) -> TI64 tail-rec. Walks key..end and
	// folds arr[k] into acc via `(acc*1009 + c) % 2147483647`.
	bS := ir.NewBuilder("summ",
		[]ir.Type{ir.TI64Array, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	sArr, sK, sAcc, sEnd := bS.Param(0), bS.Param(1), bS.Param(2), bS.Param(3)
	sDone := bS.NewBlock()
	sStep := bS.NewBlock()
	bS.CondBr(bS.LessI64(sK, sEnd), sStep, sDone)
	bS.SwitchTo(sDone)
	bS.Ret(sAcc)
	bS.SwitchTo(sStep)
	c := bS.I64ArrGet(sArr, sK)
	newAcc := bS.ModI64(bS.AddI64(bS.MulI64(sAcc, bS.ConstI64(1009)), c), bS.ConstI64(2147483647))
	nk := bS.AddI64(sK, bS.ConstI64(1))
	rec := bS.Call(summIdx, ir.TI64, sArr, nk, newAcc, sEnd)
	bS.Ret(rec)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bS.Function(),
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
