package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func powMod(x int, k int) int {
	var base int = (x % MOD)
	var exp int = k
	var result int = 1
	for (exp > 0) {
		if ((exp % 2) == 1) {
			result = (((result * base)) % MOD)
		}
		base = (((base * base)) % MOD)
		exp = (exp / 2)
	}
	return result
}

func superPow(a int, b []int) int {
	var result int = 1
	for _, digit := range b {
		result = powMod(result, 10)
		var part int = powMod(a, digit)
		result = (((result * part)) % MOD)
	}
	return result
}

func example_1() {
	expect((superPow(2, []int{3}) == 8))
}

func example_2() {
	expect((superPow(2, []int{1, 0}) == 1024))
}

func example_3() {
	expect((superPow(1, []int{4, 3, 3, 8, 5, 2}) == 1))
}

func large_numbers() {
	expect((superPow(2147483647, []int{2, 0, 0}) == 1198))
}

func zero_exponent() {
	expect((superPow(5, []int{}) == 1))
}

var MOD int = 1337
func main() {
	example_1()
	example_2()
	example_3()
	large_numbers()
	zero_exponent()
}

