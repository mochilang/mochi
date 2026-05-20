package vm3jit

// signedMagicI64 computes the magic multiplier M and shift s for
// signed division by the constant d, per Granlund-Montgomery (Hacker's
// Delight §10-4).
//
// The truncated-toward-zero quotient of `n / d` (matching Go's `/`
// semantics) is reconstructed from the result as:
//
//	hi := int64(int128(n) * M) >> 64
//	if needsCorrection { hi += n }   // M is the low 64 bits of an
//	                                  // unsigned magic that overflows
//	                                  // a signed int64
//	q  := (hi >> s) + (uint64(n) >> 63)
//
// and the remainder is `r = n - q*d`. needsCorrection is true when the
// computed unsigned magic mu is >= 2^63 (the "M < 2^63" simple shape
// from HD §10-3 does not apply). The AMD64 lowering folds the
// correction into a single `add rdx, rB` between the IMUL and the SAR
// — 3 extra bytes per magic-multiply site.
//
// ok=true is gated on d > 1 and d not a power of two (power-of-2 cases
// have cheaper arithmetic-shift lowerings the JIT can add later, so
// for now they fall back to IDIV-K). Negative d and d in {-1, 0, 1}
// also fall back to IDIV-K; the BG corpus kernels never exercise them
// and a follow-up phase can extend the predicate without changing the
// emit shape.
//
// The helper lives in a tagless file (rather than lower_amd64.go) so
// the arm64 host can unit-test the algorithm against known constants
// and against Go's `/` and `%` operators without a cross-arch build.
// AMD64 is the only backend that calls it today (Phase 6.3.4.n.2.h);
// ARM64 has cheaper SDIV+MSUB so the same gate is not worth the
// code-size cost there.
func signedMagicI64(d int64) (M int64, s uint, needsCorrection, ok bool) {
	if d <= 1 {
		return 0, 0, false, false
	}
	if d&(d-1) == 0 {
		return 0, 0, false, false
	}
	const two63 = uint64(1) << 63
	ad := uint64(d)
	t := two63
	anc := t - 1 - t%ad
	p := uint(63)
	q1 := two63 / anc
	r1 := two63 - q1*anc
	q2 := two63 / ad
	r2 := two63 - q2*ad
	var delta uint64
	for {
		p++
		q1 <<= 1
		r1 <<= 1
		if r1 >= anc {
			q1++
			r1 -= anc
		}
		q2 <<= 1
		r2 <<= 1
		if r2 >= ad {
			q2++
			r2 -= ad
		}
		delta = ad - r2
		if q1 < delta || (q1 == delta && r1 == 0) {
			continue
		}
		break
	}
	mu := q2 + 1
	needsCorrection = mu >= two63
	return int64(mu), p - 64, needsCorrection, true
}
