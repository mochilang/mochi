package corpus

import (
	"math/big"

	"mochi/compiler2/ir"
)

// BuildPidigits is the MEP-39 §6.8 parameterised BG pidigits kernel.
// The canonical BG pidigits program emits the first N decimal digits
// of pi via the Gibbons unbounded spigot algorithm, which keeps an
// arbitrary-precision state (q, r, t) plus three small ints (k, n, l)
// and either produces a digit (the "produce" branch) or refines the
// state (the "consume" branch). Iteration 1 of the cross-lang harness
// retains the exact spigot algorithm but folds the emitted digits
// into a rolling i64 hash so the cross-lang harness compares a
// single integer across peers without any string formatting.
//
// State:
//
//	q, r, t : bignum (TBigInt)
//	k, n, l : i64
//	h       : i64 rolling hash, h = (h*10 + digit) % 2147483647
//	rem     : i64 digits remaining
//
// Initial state (Gibbons):
//
//	q = 1, r = 0, t = 1, k = 1, n = 3, l = 3, h = 0
//
// Produce branch (predicate: 4*q + r - t < n*t, equivalently 4q+r < nt+t):
//
//	digit = n
//	h     = (h*10 + n) % 2147483647
//	q'    = 10*q
//	r'    = 10*(r - n*t)
//	n'    = (10*(3*q + r))/t - 10*n
//	t, k, l unchanged
//	rem'  = rem - 1
//
// Consume branch:
//
//	nr = (2*q + r)*l
//	q' = q*k
//	r' = nr
//	t' = t*l
//	n' = (q*(7*k + 2) + r*l) / (t*l)
//	k' = k+1
//	l' = l+2
//	rem unchanged
//
// We encode the kernel as a single 8-arg tail-recursive helper:
//
//	step(q, r, t, k, n, l, h, rem) -> TI64
//
// The bignum constants 0, 1, 2, 3, 4, 7, 10 are referenced via
// ConstBigInt where multiplication / subtraction with a bignum
// operand requires both sides to be bignum (the i64<->bignum bridge
// is I64ToBigInt for runtime values). Builder dedup folds equal
// literals into a single Function.BigInts entry, so each constant
// occupies one Program.Consts slot per function.
func BuildPidigits(n int64) *ir.Module {
	const stepIdx = 1

	// main(): seed (q, r, t, k, n, l, h, rem) and call step.
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	q0 := bMain.ConstBigInt("1")
	r0 := bMain.ConstBigInt("0")
	t0 := bMain.ConstBigInt("1")
	k0 := bMain.ConstI64(1)
	n0 := bMain.ConstI64(3)
	l0 := bMain.ConstI64(3)
	h0 := bMain.ConstI64(0)
	rem0 := bMain.ConstI64(n)
	res := bMain.Call(stepIdx, ir.TI64, q0, r0, t0, k0, n0, l0, h0, rem0)
	bMain.Ret(res)

	// step(q, r, t, k, n, l, h, rem) -> TI64.
	bS := ir.NewBuilder("step",
		[]ir.Type{ir.TBigInt, ir.TBigInt, ir.TBigInt, ir.TI64, ir.TI64, ir.TI64, ir.TI64, ir.TI64},
		ir.TI64)
	pQ, pR, pT := bS.Param(0), bS.Param(1), bS.Param(2)
	pK, pN, pL := bS.Param(3), bS.Param(4), bS.Param(5)
	pH, pRem := bS.Param(6), bS.Param(7)

	doneB := bS.NewBlock()
	moreB := bS.NewBlock()
	bS.CondBr(bS.LessEqI64(pRem, bS.ConstI64(0)), doneB, moreB)

	bS.SwitchTo(doneB)
	bS.Ret(pH)

	bS.SwitchTo(moreB)
	// Predicate: 4*q + r < n*t + t  (equivalent to 4q+r-t < nt without bignum subtract underflow).
	nBig := bS.I64ToBigInt(pN)
	fourQ := bS.MulBigInt(bS.ConstBigInt("4"), pQ)
	lhs := bS.AddBigInt(fourQ, pR)
	ntProd := bS.MulBigInt(nBig, pT)
	rhs := bS.AddBigInt(ntProd, pT)
	cond := bS.LessBigInt(lhs, rhs)

	produceB := bS.NewBlock()
	consumeB := bS.NewBlock()
	bS.CondBr(cond, produceB, consumeB)

	// Produce branch.
	bS.SwitchTo(produceB)
	tenBigP := bS.ConstBigInt("10")
	newQ := bS.MulBigInt(tenBigP, pQ)
	// r - n*t : nBig and pT may be reused since IR is SSA-ish; rebuild for clarity.
	nBigP := bS.I64ToBigInt(pN)
	ntProdP := bS.MulBigInt(nBigP, pT)
	rMinus := bS.SubBigInt(pR, ntProdP)
	newR := bS.MulBigInt(tenBigP, rMinus)
	// n' = (10*(3q + r))/t - 10n
	threeBig := bS.ConstBigInt("3")
	threeQ := bS.MulBigInt(threeBig, pQ)
	threeQplusR := bS.AddBigInt(threeQ, pR)
	tenTimes := bS.MulBigInt(tenBigP, threeQplusR)
	div := bS.DivBigInt(tenTimes, pT)
	// 10*n in i64 fits for any reasonable run (n stays bounded near a digit value, but
	// the Gibbons invariant ensures 0 <= n < 10 after each produce-branch update; to
	// keep things robust before that invariant is established we still go via bignum
	// subtraction, then convert back).
	tenN := bS.MulI64(pN, bS.ConstI64(10))
	tenNBig := bS.I64ToBigInt(tenN)
	newNBig := bS.SubBigInt(div, tenNBig)
	newN := bS.BigIntToI64(newNBig)
	// h' = (h*10 + n) % 2147483647
	newH := bS.ModI64(bS.AddI64(bS.MulI64(pH, bS.ConstI64(10)), pN), bS.ConstI64(2147483647))
	// rem' = rem - 1
	newRem := bS.SubI64(pRem, bS.ConstI64(1))
	recP := bS.Call(stepIdx, ir.TI64, newQ, newR, pT, pK, newN, pL, newH, newRem)
	bS.Ret(recP)

	// Consume branch.
	bS.SwitchTo(consumeB)
	kBig := bS.I64ToBigInt(pK)
	lBig := bS.I64ToBigInt(pL)
	// nr = (2*q + r)*l
	twoBig := bS.ConstBigInt("2")
	twoQ := bS.MulBigInt(twoBig, pQ)
	twoQplusR := bS.AddBigInt(twoQ, pR)
	nrC := bS.MulBigInt(twoQplusR, lBig)
	// q' = q*k
	newQC := bS.MulBigInt(pQ, kBig)
	// t' = t*l
	newTC := bS.MulBigInt(pT, lBig)
	// n' = (q*(7*k+2) + r*l) / (t*l)
	sevenKplus2 := bS.AddI64(bS.MulI64(pK, bS.ConstI64(7)), bS.ConstI64(2))
	sevenKplus2Big := bS.I64ToBigInt(sevenKplus2)
	qTerm := bS.MulBigInt(pQ, sevenKplus2Big)
	rL := bS.MulBigInt(pR, lBig)
	numer := bS.AddBigInt(qTerm, rL)
	newNCBig := bS.DivBigInt(numer, newTC)
	newNC := bS.BigIntToI64(newNCBig)
	// k' = k+1, l' = l+2
	newKC := bS.AddI64(pK, bS.ConstI64(1))
	newLC := bS.AddI64(pL, bS.ConstI64(2))
	recC := bS.Call(stepIdx, ir.TI64, newQC, nrC, newTC, newKC, newNC, newLC, pH, pRem)
	bS.Ret(recC)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bS.Function()}, Main: 0}
}

// ExpectPidigits runs the same Gibbons spigot in plain Go using
// math/big so the corpus test can assert vm2 produces the same i64
// rolling hash over the first N digits of pi.
func ExpectPidigits(n int64) int64 {
	q := big.NewInt(1)
	r := big.NewInt(0)
	t := big.NewInt(1)
	k := int64(1)
	nd := int64(3)
	l := int64(3)
	var h int64
	rem := n

	four := big.NewInt(4)
	three := big.NewInt(3)
	ten := big.NewInt(10)
	two := big.NewInt(2)
	mod := int64(2147483647)

	for rem > 0 {
		// predicate: 4q + r < nt + t
		lhs := new(big.Int).Mul(four, q)
		lhs.Add(lhs, r)
		nbig := big.NewInt(nd)
		ntp := new(big.Int).Mul(nbig, t)
		rhs := new(big.Int).Add(ntp, t)
		if lhs.Cmp(rhs) < 0 {
			// produce
			h = (h*10 + nd) % mod
			newQ := new(big.Int).Mul(ten, q)
			rMinus := new(big.Int).Sub(r, ntp)
			newR := new(big.Int).Mul(ten, rMinus)
			threeQplusR := new(big.Int).Mul(three, q)
			threeQplusR.Add(threeQplusR, r)
			tenTimes := new(big.Int).Mul(ten, threeQplusR)
			div := new(big.Int).Quo(tenTimes, t)
			tenN := big.NewInt(10 * nd)
			newNbig := new(big.Int).Sub(div, tenN)
			q = newQ
			r = newR
			nd = newNbig.Int64()
			rem--
		} else {
			// consume
			kbig := big.NewInt(k)
			lbig := big.NewInt(l)
			twoQ := new(big.Int).Mul(two, q)
			twoQplusR := new(big.Int).Add(twoQ, r)
			nr := new(big.Int).Mul(twoQplusR, lbig)
			newQ := new(big.Int).Mul(q, kbig)
			newT := new(big.Int).Mul(t, lbig)
			sevenKplus2 := big.NewInt(7*k + 2)
			qTerm := new(big.Int).Mul(q, sevenKplus2)
			rL := new(big.Int).Mul(r, lbig)
			numer := new(big.Int).Add(qTerm, rL)
			newNbig := new(big.Int).Quo(numer, newT)
			q = newQ
			r = nr
			t = newT
			nd = newNbig.Int64()
			k++
			l += 2
		}
	}
	return h
}
