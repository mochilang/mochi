package vm3jit

// pow2ShiftI64 reports whether d is a positive power of two and, if so,
// returns the shift count k (so d == 1<<k, 1 <= k <= 62). Power-of-1
// (d=1) is rejected because the identity case is uninteresting and the
// shift lowering assumes k >= 1. d > 1<<62 is rejected so the
// arithmetic-shift count stays inside the 64-bit signed range.
//
// Phase 6.3.4.n.13. The companion AMD64 emitter
// (emitDivKOrModKPow2AMD64) replaces the ~30-cycle IDIV-K with a 5-op
// sar/shr/add/sar sequence executing in ~5 cycles. This is the same
// transformation LLVM, GCC, and Go's reference compiler apply for
// power-of-2 constant divisors. The helper lives in a tagless file so
// the arm64 host can unit-test the predicate against Go's `/` and `%`
// without a cross-arch build.
func pow2ShiftI64(d int64) (uint, bool) {
	if d <= 1 {
		return 0, false
	}
	if d&(d-1) != 0 {
		return 0, false
	}
	k := uint(0)
	for v := d; v > 1; v >>= 1 {
		k++
	}
	if k < 1 || k > 62 {
		return 0, false
	}
	return k, true
}
