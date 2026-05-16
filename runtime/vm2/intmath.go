package vm2

import "math"

// addOverflow returns a+b and ok=true when the result fits in int64.
// ok=false means signed overflow occurred and the caller should fall
// through to a slower path (float promotion or error).
func addOverflow(a, b int64) (int64, bool) {
	r := a + b
	if (a > 0 && b > 0 && r < a) || (a < 0 && b < 0 && r > a) {
		return 0, false
	}
	return r, true
}

func subOverflow(a, b int64) (int64, bool) {
	r := a - b
	if (b > 0 && r > a) || (b < 0 && r < a) {
		return 0, false
	}
	return r, true
}

func mulOverflow(a, b int64) (int64, bool) {
	if a == 0 || b == 0 {
		return 0, true
	}
	if a == math.MinInt64 || b == math.MinInt64 {
		if a == 1 {
			return b, true
		}
		if b == 1 {
			return a, true
		}
		return 0, false
	}
	r := a * b
	if r/b != a {
		return 0, false
	}
	return r, true
}
