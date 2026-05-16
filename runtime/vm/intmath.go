package vm

// Native-int fast paths for the arithmetic opcodes. The general handlers
// fall through to big.Int when these report overflow.

func addIntOverflow(a, b int) (int, bool) {
	r := a + b
	if (a^r)&(b^r) < 0 {
		return 0, false
	}
	return r, true
}

func subIntOverflow(a, b int) (int, bool) {
	r := a - b
	if (a^b)&(a^r) < 0 {
		return 0, false
	}
	return r, true
}

func mulIntOverflow(a, b int) (int, bool) {
	if a == 0 || b == 0 {
		return 0, true
	}
	r := a * b
	if r/b != a {
		return 0, false
	}
	return r, true
}
