// Package corpus exposes the MEP-17 / MEP-23 benchmark suite expressed
// directly in compiler2 IR. Each builder returns an ir.Module whose
// Main entry computes the same i64 result the original .mochi program
// would print, parametrized by N where applicable.
//
// Loops are expressed as tail-recursive helpers (no SSA phi) so the
// modules round-trip through emit without needing phi lowering. The
// opt.TailCall pass is expected to rewrite the recursive call sites
// before emission so deep iterations do not grow the frame stack.
//
// Coverage is the int64-feasible subset of the corpus. Programs that
// depend on strings, lists, maps, structs, closures, or query
// iteration are deferred until those subsystems land in vm2.
package corpus

import "mochi/compiler2/ir"

// Program is one corpus entry: a builder that produces an ir.Module
// for a given N, plus an oracle that computes the same result in
// plain Go so tests can assert equivalence without a second VM.
type Program struct {
	Name   string
	Build  func(n int64) *ir.Module
	Expect func(n int64) int64
}

// All returns the corpus in a stable order.
func All() []Program {
	return []Program{
		{Name: "fib", Build: BuildFibRec, Expect: ExpectFibRec},
		{Name: "iter_sum", Build: BuildIterSum, Expect: ExpectIterSum},
		{Name: "math_sum_loop", Build: BuildSumLoop, Expect: ExpectSumLoop},
		{Name: "math_mul_loop", Build: BuildMulLoop, Expect: ExpectMulLoop},
		{Name: "math_fact_rec", Build: BuildFactRec, Expect: ExpectFactRec},
		{Name: "math_fib_iter", Build: BuildFibIter, Expect: ExpectFibIter},
		{Name: "math_fib_rec", Build: BuildFibRec, Expect: ExpectFibRec},
		{Name: "math_prime_count", Build: BuildPrimeCount, Expect: ExpectPrimeCount},
		{Name: "strings_concat_loop", Build: BuildStringsConcatLoop, Expect: ExpectStringsConcatLoop},
		{Name: "lists_fill_sum", Build: BuildListsFillSum, Expect: ExpectListsFillSum},
		{Name: "maps_fill_sum", Build: BuildMapsFillSum, Expect: ExpectMapsFillSum},
	}
}

// ---------- fib_rec (also runtime/vm/bench fib) ----------

// BuildFibRec builds the naive doubly-recursive fibonacci:
//
//	fib(n) = if n <= 1 then n else fib(n-1) + fib(n-2)
//	main() = fib(N)
func BuildFibRec(n int64) *ir.Module {
	const fibIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	r := bMain.Call(fibIdx, ir.TI64, nv)
	bMain.Ret(r)

	bFib := ir.NewBuilder("fib", []ir.Type{ir.TI64}, ir.TI64)
	pN := bFib.Param(0)
	thenB := bFib.NewBlock()
	elseB := bFib.NewBlock()
	one := bFib.ConstI64(1)
	cond := bFib.LessEqI64(pN, one)
	bFib.CondBr(cond, thenB, elseB)

	bFib.SwitchTo(thenB)
	bFib.Ret(pN)

	bFib.SwitchTo(elseB)
	nm1 := bFib.SubI64(pN, one)
	two := bFib.ConstI64(2)
	nm2 := bFib.SubI64(pN, two)
	a := bFib.Call(fibIdx, ir.TI64, nm1)
	b := bFib.Call(fibIdx, ir.TI64, nm2)
	s := bFib.AddI64(a, b)
	bFib.Ret(s)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bFib.Function()}, Main: 0}
}

// ExpectFibRec computes fib(n) by the same recurrence.
func ExpectFibRec(n int64) int64 {
	a, b := int64(0), int64(1)
	for i := int64(0); i < n; i++ {
		a, b = b, a+b
	}
	return a
}

// ---------- iter_sum ----------

// BuildIterSum builds `for i in 1..N { total += i }` returning total.
// Loop encoded as tail-recursive helper(i, total, n).
func BuildIterSum(n int64) *ir.Module {
	const helperIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	one := bMain.ConstI64(1)
	zero := bMain.ConstI64(0)
	nv := bMain.ConstI64(n)
	r := bMain.Call(helperIdx, ir.TI64, one, zero, nv)
	bMain.Ret(r)

	bH := ir.NewBuilder("iter_sum_loop", []ir.Type{ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	pI, pT, pN := bH.Param(0), bH.Param(1), bH.Param(2)
	done := bH.NewBlock()
	step := bH.NewBlock()
	cond := bH.LessI64(pI, pN)
	bH.CondBr(cond, step, done)

	bH.SwitchTo(done)
	bH.Ret(pT)

	bH.SwitchTo(step)
	c1 := bH.ConstI64(1)
	ni := bH.AddI64(pI, c1)
	nt := bH.AddI64(pT, pI)
	r2 := bH.Call(helperIdx, ir.TI64, ni, nt, pN)
	bH.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bH.Function()}, Main: 0}
}

// ExpectIterSum: sum of [1, n).
func ExpectIterSum(n int64) int64 { return (n - 1) * n / 2 }

// ---------- math/sum_loop ----------

// BuildSumLoop = iter_sum but the original benchmark wraps the loop in
// a function called `sum_loop` and invokes it from main. Shape matches
// the .mochi template: sum_loop(N).
func BuildSumLoop(n int64) *ir.Module { return BuildIterSum(n) }

// ExpectSumLoop matches ExpectIterSum.
func ExpectSumLoop(n int64) int64 { return ExpectIterSum(n) }

// ---------- math/mul_loop ----------

// BuildMulLoop builds `for i in 1..N { result *= i }` returning result
// (initial 1). Equivalent to (N-1)!.
func BuildMulLoop(n int64) *ir.Module {
	const helperIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	one := bMain.ConstI64(1)
	one2 := bMain.ConstI64(1)
	nv := bMain.ConstI64(n)
	r := bMain.Call(helperIdx, ir.TI64, one, one2, nv)
	bMain.Ret(r)

	bH := ir.NewBuilder("mul_loop", []ir.Type{ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	pI, pR, pN := bH.Param(0), bH.Param(1), bH.Param(2)
	done := bH.NewBlock()
	step := bH.NewBlock()
	cond := bH.LessI64(pI, pN)
	bH.CondBr(cond, step, done)

	bH.SwitchTo(done)
	bH.Ret(pR)

	bH.SwitchTo(step)
	c1 := bH.ConstI64(1)
	ni := bH.AddI64(pI, c1)
	nr := bH.MulI64(pR, pI)
	r2 := bH.Call(helperIdx, ir.TI64, ni, nr, pN)
	bH.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bH.Function()}, Main: 0}
}

// ExpectMulLoop = product of [1, n) = (n-1)!.
func ExpectMulLoop(n int64) int64 {
	r := int64(1)
	for i := int64(1); i < n; i++ {
		r *= i
	}
	return r
}

// ---------- math/fact_rec ----------

// BuildFactRec is the textbook recursive factorial, NOT tail-recursive,
// to match the original benchmark's frame-growth profile.
//
//	fact(n) = if n == 0 then 1 else n * fact(n-1)
func BuildFactRec(n int64) *ir.Module {
	const factIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	r := bMain.Call(factIdx, ir.TI64, nv)
	bMain.Ret(r)

	bF := ir.NewBuilder("fact_rec", []ir.Type{ir.TI64}, ir.TI64)
	pN := bF.Param(0)
	zero := bF.ConstI64(0)
	cond := bF.EqualI64(pN, zero)
	thenB := bF.NewBlock()
	elseB := bF.NewBlock()
	bF.CondBr(cond, thenB, elseB)

	bF.SwitchTo(thenB)
	one := bF.ConstI64(1)
	bF.Ret(one)

	bF.SwitchTo(elseB)
	one2 := bF.ConstI64(1)
	nm1 := bF.SubI64(pN, one2)
	sub := bF.Call(factIdx, ir.TI64, nm1)
	r2 := bF.MulI64(pN, sub)
	bF.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bF.Function()}, Main: 0}
}

// ExpectFactRec computes n!.
func ExpectFactRec(n int64) int64 {
	r := int64(1)
	for i := int64(2); i <= n; i++ {
		r *= i
	}
	return r
}

// ---------- math/fib_iter ----------

// BuildFibIter is the iterative fibonacci `(a, b) -> (b, a+b)` repeated
// N times, returning a. Loop encoded as tail-recursive helper.
//
//	helper(i, a, b, n) = if i >= n then a else helper(i+1, b, a+b, n)
func BuildFibIter(n int64) *ir.Module {
	const helperIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	zero := bMain.ConstI64(0)
	a0 := bMain.ConstI64(0)
	b0 := bMain.ConstI64(1)
	nv := bMain.ConstI64(n)
	r := bMain.Call(helperIdx, ir.TI64, zero, a0, b0, nv)
	bMain.Ret(r)

	bH := ir.NewBuilder("fib_iter_loop", []ir.Type{ir.TI64, ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	pI, pA, pB, pN := bH.Param(0), bH.Param(1), bH.Param(2), bH.Param(3)
	done := bH.NewBlock()
	step := bH.NewBlock()
	cond := bH.LessI64(pI, pN)
	bH.CondBr(cond, step, done)

	bH.SwitchTo(done)
	bH.Ret(pA)

	bH.SwitchTo(step)
	one := bH.ConstI64(1)
	ni := bH.AddI64(pI, one)
	nab := bH.AddI64(pA, pB)
	r2 := bH.Call(helperIdx, ir.TI64, ni, pB, nab, pN)
	bH.Ret(r2)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bH.Function()}, Main: 0}
}

// ExpectFibIter matches BuildFibRec's value semantics.
func ExpectFibIter(n int64) int64 { return ExpectFibRec(n) }

// ---------- math/prime_count ----------

// BuildPrimeCount counts primes in [2, N). Encoded as four functions:
//
//	is_prime(n)            = if n < 2 then false else is_prime_loop(n, 2)
//	is_prime_loop(n, i)    = if i >= n-1 then true
//	                         else if n%i == 0 then false
//	                         else is_prime_loop(n, i+1)
//	count_loop(n, i, acc)  = if i >= n then acc
//	                         else if is_prime(i)
//	                              then count_loop(n, i+1, acc+1)
//	                              else count_loop(n, i+1, acc)
//	main()                 = count_loop(N, 2, 0)
func BuildPrimeCount(n int64) *ir.Module {
	const (
		mainIdx        = 0
		isPrimeIdx     = 1
		isPrimeLoopIdx = 2
		countLoopIdx   = 3
	)
	_ = mainIdx

	// main
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	i0 := bMain.ConstI64(2)
	acc0 := bMain.ConstI64(0)
	r := bMain.Call(countLoopIdx, ir.TI64, nv, i0, acc0)
	bMain.Ret(r)

	// is_prime(n): returns 1 if prime, 0 otherwise (as i64 for shape simplicity).
	bIP := ir.NewBuilder("is_prime", []ir.Type{ir.TI64}, ir.TI64)
	pN := bIP.Param(0)
	two := bIP.ConstI64(2)
	cond := bIP.LessI64(pN, two)
	tooSmall := bIP.NewBlock()
	bigEnough := bIP.NewBlock()
	bIP.CondBr(cond, tooSmall, bigEnough)

	bIP.SwitchTo(tooSmall)
	zero := bIP.ConstI64(0)
	bIP.Ret(zero)

	bIP.SwitchTo(bigEnough)
	twoStart := bIP.ConstI64(2)
	r2 := bIP.Call(isPrimeLoopIdx, ir.TI64, pN, twoStart)
	bIP.Ret(r2)

	// is_prime_loop(n, i)
	bL := ir.NewBuilder("is_prime_loop", []ir.Type{ir.TI64, ir.TI64}, ir.TI64)
	lN, lI := bL.Param(0), bL.Param(1)
	one := bL.ConstI64(1)
	limit := bL.SubI64(lN, one)
	cExit := bL.LessI64(lI, limit) // true while i < n-1
	doneB := bL.NewBlock()
	moreB := bL.NewBlock()
	bL.CondBr(cExit, moreB, doneB)

	bL.SwitchTo(doneB)
	primeYes := bL.ConstI64(1)
	bL.Ret(primeYes)

	bL.SwitchTo(moreB)
	rem := bL.ModI64(lN, lI)
	zeroL := bL.ConstI64(0)
	divides := bL.EqualI64(rem, zeroL)
	notPrime := bL.NewBlock()
	keepGoing := bL.NewBlock()
	bL.CondBr(divides, notPrime, keepGoing)

	bL.SwitchTo(notPrime)
	zeroR := bL.ConstI64(0)
	bL.Ret(zeroR)

	bL.SwitchTo(keepGoing)
	oneInc := bL.ConstI64(1)
	ni := bL.AddI64(lI, oneInc)
	r3 := bL.Call(isPrimeLoopIdx, ir.TI64, lN, ni)
	bL.Ret(r3)

	// count_loop(n, i, acc)
	bC := ir.NewBuilder("count_loop", []ir.Type{ir.TI64, ir.TI64, ir.TI64}, ir.TI64)
	cN, cI, cAcc := bC.Param(0), bC.Param(1), bC.Param(2)
	cCond := bC.LessI64(cI, cN)
	cDone := bC.NewBlock()
	cStep := bC.NewBlock()
	bC.CondBr(cCond, cStep, cDone)

	bC.SwitchTo(cDone)
	bC.Ret(cAcc)

	bC.SwitchTo(cStep)
	isP := bC.Call(isPrimeIdx, ir.TI64, cI)
	oneIs := bC.ConstI64(1)
	isPrime := bC.EqualI64(isP, oneIs)
	primeBranch := bC.NewBlock()
	nonPrimeBranch := bC.NewBlock()
	bC.CondBr(isPrime, primeBranch, nonPrimeBranch)

	bC.SwitchTo(primeBranch)
	oneA := bC.ConstI64(1)
	ni2 := bC.AddI64(cI, oneA)
	nacc := bC.AddI64(cAcc, oneA)
	r4 := bC.Call(countLoopIdx, ir.TI64, cN, ni2, nacc)
	bC.Ret(r4)

	bC.SwitchTo(nonPrimeBranch)
	oneB := bC.ConstI64(1)
	ni3 := bC.AddI64(cI, oneB)
	r5 := bC.Call(countLoopIdx, ir.TI64, cN, ni3, cAcc)
	bC.Ret(r5)

	return &ir.Module{Funcs: []*ir.Function{
		bMain.Function(),
		bIP.Function(),
		bL.Function(),
		bC.Function(),
	}, Main: 0}
}

// ExpectPrimeCount counts primes in [2, n).
func ExpectPrimeCount(n int64) int64 {
	count := int64(0)
	for k := int64(2); k < n; k++ {
		prime := true
		for i := int64(2); i < k-1; i++ {
			if k%i == 0 {
				prime = false
				break
			}
		}
		if k >= 2 && prime {
			count++
		}
	}
	return count
}
