package vm3jit

import "testing"

// TestSignedMagicI64KnownConstants verifies the Granlund-Montgomery
// magic multiplier matches well-known reference values. The algorithm
// minimizes the shift count s, so 6 = 2*3 ends up with the half-magic
// of 3 and s=0 rather than M(3)*2 and s=1.
func TestSignedMagicI64KnownConstants(t *testing.T) {
	cases := []struct {
		d        int64
		wantM    int64
		wantS    uint
		wantCorr bool
		wantOk   bool
		comment  string
	}{
		{3, 0x5555555555555556, 0, false, true, "fundamental"},
		{5, 0x6666666666666667, 1, false, true, "fundamental"},
		{6, 0x2AAAAAAAAAAAAAAB, 0, false, true, "= 2*3, minimized"},
		{7, 0x4924924924924925, 1, false, true, "fannkuch init loop"},
		{9, 0x1C71C71C71C71C72, 0, false, true, "fundamental, minimized"},
		{10, 0x6666666666666667, 2, false, true, "= 2*5"},
		{11, 0x2E8BA2E8BA2E8BA3, 1, false, true, "fundamental"},
		// 100, 1000, etc. fall into the correction-needed shape
		// (unsigned magic overflows int64).
		{100, -0x5C28F5C28F5C28F5, 6, true, true, "needs correction"},
		{0, 0, 0, false, false, "divide-by-zero falls through"},
		{1, 0, 0, false, false, "identity falls through"},
		{2, 0, 0, false, false, "power of 2 -> shift fallback"},
		{4, 0, 0, false, false, "power of 2 -> shift fallback"},
		{8, 0, 0, false, false, "power of 2 -> shift fallback"},
		{-1, 0, 0, false, false, "negative falls through"},
		{-7, 0, 0, false, false, "negative falls through"},
	}
	for _, c := range cases {
		gotM, gotS, gotCorr, gotOk := signedMagicI64(c.d)
		if gotOk != c.wantOk {
			t.Errorf("d=%d (%s): ok got %v want %v", c.d, c.comment, gotOk, c.wantOk)
			continue
		}
		if !gotOk {
			continue
		}
		if gotM != c.wantM || gotS != c.wantS || gotCorr != c.wantCorr {
			t.Errorf("d=%d (%s): got M=%#x s=%d corr=%v, want M=%#x s=%d corr=%v",
				c.d, c.comment, uint64(gotM), gotS, gotCorr,
				uint64(c.wantM), c.wantS, c.wantCorr)
		}
	}
}

// TestSignedMagicI64MatchesGoDiv exhaustively verifies that for every
// d where signedMagicI64 returns ok=true, the magic-multiply formula
// reproduces Go's truncated `n / d` and `n % d` over a representative
// set of n values including the int64 extremes. This covers both the
// simple shape (corr=false) and the correction shape (corr=true).
func TestSignedMagicI64MatchesGoDiv(t *testing.T) {
	ds := []int64{3, 5, 6, 7, 9, 10, 11, 13, 17, 100, 1000, 7919, (1 << 30) + 1, (1 << 62) - 17}
	ns := []int64{
		0, 1, -1, 2, -2, 7, -7, 1023, -1023,
		99, -99, 100, -100, 101, -101, 1000, -1000,
		1<<31 - 1, -(1 << 31), 1 << 32, -(1 << 32),
		1<<62 - 1, -(1 << 62),
		1<<63 - 1, -(1 << 63),
		3141592653589793238, -3141592653589793238,
	}
	for _, d := range ds {
		M, s, corr, ok := signedMagicI64(d)
		if !ok {
			t.Fatalf("d=%d: signedMagicI64 ok=false (expected non-trivial)", d)
		}
		for _, n := range ns {
			hi := mul128Hi(n, M)
			if corr {
				hi += n
			}
			gotQ := hi >> s
			gotQ += int64(uint64(n) >> 63)
			gotR := n - gotQ*d
			wantQ := n / d
			wantR := n % d
			if gotQ != wantQ || gotR != wantR {
				t.Errorf("d=%d n=%d: got q=%d r=%d want q=%d r=%d (M=%#x s=%d corr=%v)",
					d, n, gotQ, gotR, wantQ, wantR, uint64(M), s, corr)
			}
		}
	}
}

// mul128Hi returns the high 64 bits of the signed 128-bit product
// a*b. Pure-Go, used only by the magic-multiply unit test.
func mul128Hi(a, b int64) int64 {
	const mask32 = uint64(1<<32 - 1)
	ua := uint64(a)
	ub := uint64(b)
	aLo := ua & mask32
	aHi := ua >> 32
	bLo := ub & mask32
	bHi := ub >> 32

	loLo := aLo * bLo
	hiLo := aHi * bLo
	loHi := aLo * bHi
	hiHi := aHi * bHi

	mid := (loLo >> 32) + (hiLo & mask32) + (loHi & mask32)
	hi := hiHi + (hiLo >> 32) + (loHi >> 32) + (mid >> 32)

	if a < 0 {
		hi -= uint64(b)
	}
	if b < 0 {
		hi -= uint64(a)
	}
	return int64(hi)
}
