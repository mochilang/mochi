package vm3jit

import "testing"

// TestPow2ShiftI64Recognition verifies pow2ShiftI64 accepts the
// 2^k constants for k in [1, 62] and rejects everything else.
func TestPow2ShiftI64Recognition(t *testing.T) {
	cases := []struct {
		d      int64
		wantK  uint
		wantOk bool
	}{
		{0, 0, false},
		{1, 0, false},
		{2, 1, true},
		{3, 0, false},
		{4, 2, true},
		{6, 0, false},
		{7, 0, false},
		{8, 3, true},
		{16, 4, true},
		{1 << 30, 30, true},
		{1 << 62, 62, true},
		{int64(-1) << 63, 0, false},
		{-1, 0, false},
		{-2, 0, false},
		{-4, 0, false},
	}
	for _, c := range cases {
		gotK, gotOk := pow2ShiftI64(c.d)
		if gotOk != c.wantOk {
			t.Errorf("d=%d: ok got %v want %v", c.d, gotOk, c.wantOk)
			continue
		}
		if !gotOk {
			continue
		}
		if gotK != c.wantK {
			t.Errorf("d=%d: k got %d want %d", c.d, gotK, c.wantK)
		}
	}
}

// TestPow2ShiftI64FormulaMatchesGo simulates the pow2 shift sequence
// the AMD64 emitter generates and confirms it reproduces Go's signed
// `/` and `%` for every accepted divisor across a wide range of n
// (including int64 extremes). Catches sign-correction bugs in the
// shr/sar/add chain before they reach a hot loop.
func TestPow2ShiftI64FormulaMatchesGo(t *testing.T) {
	ds := []int64{2, 4, 8, 16, 32, 64, 1024, 1 << 30, 1 << 40, 1 << 62}
	ns := []int64{
		0, 1, -1, 2, -2, 7, -7, 1023, -1023,
		99, -99, 100, -100, 101, -101, 1000, -1000,
		1<<31 - 1, -(1 << 31), 1 << 32, -(1 << 32),
		1<<62 - 1, -(1 << 62),
		1<<63 - 1, -(1 << 63),
		3141592653589793238, -3141592653589793238,
	}
	for _, d := range ds {
		k, ok := pow2ShiftI64(d)
		if !ok {
			t.Fatalf("d=%d: pow2ShiftI64 ok=false (expected true)", d)
		}
		for _, n := range ns {
			// Mirror the AMD64 emit sequence in scalar Go:
			//   r = n
			//   r = r >> 63   (arith, gives 0 or -1)
			//   r = uint64(r) >> (64-k)  (logical, gives 0 or 2^k - 1)
			//   r = n + r
			//   q = r >> k    (arith)
			//   m = n - (q << k)
			signMask := n >> 63
			corr := int64(uint64(signMask) >> (64 - k))
			gotQ := (n + corr) >> k
			gotR := n - (gotQ << k)
			wantQ := n / d
			wantR := n % d
			if gotQ != wantQ || gotR != wantR {
				t.Errorf("d=%d n=%d: got q=%d r=%d want q=%d r=%d (k=%d)",
					d, n, gotQ, gotR, wantQ, wantR, k)
			}
		}
	}
}
