package corpus

import "mochi/compiler2/ir"

// BuildRegexRedux is the MEP-39 §6.9 parameterised BG regex_redux kernel.
// vm2 has no regex ops (see §4.2.4 path (b)), so the kernel is a plain
// state-machine scan: generate a deterministic DNA stream of N codes
// 0..3 via the same fasta LCG, maintain a 4-byte rolling window packed
// 2-bits-per-code into an i64, and count matches against the two
// alternation patterns "agtt" and "ttga". Output is the match count,
// integer-comparable across every peer without string formatting.
//
// Encoding: a=0, c=1, g=2, t=3. The window stores the last 4 codes
// big-endian (oldest in bits 7..6, newest in bits 1..0); each step
// does window = ((window << 2) & 0xFF) | code. After i >= 3 the
// window holds a full 4-byte view of the input, and:
//
//	"agtt" = 0,2,3,3 → 0b00101111 = 47
//	"ttga" = 3,3,2,0 → 0b11111000 = 248
//
// The per-iter code is derived from the high bits of the LCG seed
// rather than seed%4. The fasta LCG has modulus 139968 = 2^6 * 3^7,
// so gcd(139968, 4) = 4 and seed%4 cycles in 4 steps (the entire
// stream becomes periodic and patterns never match). Bucketing the
// seed into four equal sub-ranges via (seed * 4) / 139968 instead
// gives a uniform 0..3 code per step, and the LCG's permutation of
// the seed space scrambles which bucket the code lands in.
//
// Iteration 1 scope. The BG canonical regex_redux uses 9 length-8
// alternation patterns over a 5 MB DNA input. At our cross-lang sizes
// (N up to 10000) length-8 patterns match too rarely (~0.3 matches
// per 10k bases on uniform input) to be a stable signal. Length-4
// patterns give ~75 matches at N=10000, which is robust against LCG
// distribution skew and keeps the count meaningful. Iteration 2
// will widen patterns to length 8 and bump N to the BG canonical
// size; iteration 3 adds the 9-pattern set + IUB substitutions.
//
// Three IR functions:
//
//	main()                            = loop(42, 0, 0, 0, N); return count
//	loop(seed, win, cnt, i, n) -> TI64 = tail-rec, one LCG step + one window shift + one match check
//
// The match check is inlined into the loop body as a pair of i64
// compares; no separate function is needed because the two patterns
// are constants.
func BuildRegexRedux(n int64) *ir.Module {
	const loopIdx = 1

	// main(): call loop(42, 0, 0, 0, N), return its result.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	seed0 := bMain.ConstI64(42)
	win0 := bMain.ConstI64(0)
	cnt0 := bMain.ConstI64(0)
	i0 := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	r := bMain.Call(loopIdx, ir.TI64, seed0, win0, cnt0, i0, nv)
	bMain.Ret(r)

	// loop(seed, win, cnt, i, n):
	bL := ir.NewBuilder("loop",
		[]ir.Type{ir.TI64, ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	pSeed, pWin, pCnt, pI, pN := bL.Param(0), bL.Param(1), bL.Param(2), bL.Param(3), bL.Param(4)
	lDone := bL.NewBlock()
	lStep := bL.NewBlock()
	bL.CondBr(bL.LessI64(pI, pN), lStep, lDone)
	bL.SwitchTo(lDone)
	bL.Ret(pCnt)
	bL.SwitchTo(lStep)
	// LCG: new_seed = (seed * 3877 + 29573) % 139968 (same as fasta).
	mul := bL.MulI64(pSeed, bL.ConstI64(3877))
	add := bL.AddI64(mul, bL.ConstI64(29573))
	newSeed := bL.ModI64(add, bL.ConstI64(139968))
	// code = (new_seed * 4) / 139968  (high-bits bucketing; see doc comment)
	codeNum := bL.MulI64(newSeed, bL.ConstI64(4))
	code := bL.DivI64(codeNum, bL.ConstI64(139968))
	// new_win = ((win << 2) & 0xFF) | code → implemented as ((win*4) mod 256) + code
	winShift := bL.MulI64(pWin, bL.ConstI64(4))
	winMasked := bL.ModI64(winShift, bL.ConstI64(256))
	newWin := bL.AddI64(winMasked, code)
	// match check: only count when at least 4 codes have been emitted (i >= 3).
	armCheck := bL.NewBlock()
	skipCheck := bL.NewBlock()
	bL.CondBr(bL.LessI64(pI, bL.ConstI64(3)), skipCheck, armCheck)

	bL.SwitchTo(skipCheck)
	recSkip := bL.Call(loopIdx, ir.TI64,
		newSeed, newWin, pCnt, bL.AddI64(pI, bL.ConstI64(1)), pN)
	bL.Ret(recSkip)

	bL.SwitchTo(armCheck)
	// Match against "agtt" (47) or "ttga" (248).
	mAgtt := bL.NewBlock()
	checkTtga := bL.NewBlock()
	mTtga := bL.NewBlock()
	noMatch := bL.NewBlock()
	bL.CondBr(bL.EqualI64(newWin, bL.ConstI64(47)), mAgtt, checkTtga)

	bL.SwitchTo(checkTtga)
	bL.CondBr(bL.EqualI64(newWin, bL.ConstI64(248)), mTtga, noMatch)

	bL.SwitchTo(mAgtt)
	recAgtt := bL.Call(loopIdx, ir.TI64,
		newSeed, newWin, bL.AddI64(pCnt, bL.ConstI64(1)),
		bL.AddI64(pI, bL.ConstI64(1)), pN)
	bL.Ret(recAgtt)

	bL.SwitchTo(mTtga)
	recTtga := bL.Call(loopIdx, ir.TI64,
		newSeed, newWin, bL.AddI64(pCnt, bL.ConstI64(1)),
		bL.AddI64(pI, bL.ConstI64(1)), pN)
	bL.Ret(recTtga)

	bL.SwitchTo(noMatch)
	recNone := bL.Call(loopIdx, ir.TI64,
		newSeed, newWin, pCnt,
		bL.AddI64(pI, bL.ConstI64(1)), pN)
	bL.Ret(recNone)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(), bL.Function(),
	}, Main: 0}
}

// ExpectRegexRedux runs the same algorithm in plain Go so the oracle
// test can assert vm2 produces the bit-identical match count.
func ExpectRegexRedux(n int64) int64 {
	seed := int64(42)
	window := int64(0)
	count := int64(0)
	for i := int64(0); i < n; i++ {
		seed = (seed*3877 + 29573) % 139968
		code := (seed * 4) / 139968
		window = ((window << 2) & 0xFF) | code
		if i >= 3 {
			if window == 47 || window == 248 {
				count++
			}
		}
	}
	return count
}
