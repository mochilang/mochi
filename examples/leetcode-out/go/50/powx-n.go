package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func myPow(x float64, n int) float64 {
	if (n == 0) {
		return 1
	}
	if (n < 0) {
		return (1 / myPow(x, -n))
	}
	var base float64 = x
	var exp int = n
	var result float64 = 1
	for (exp > 0) {
		if ((exp % 2) == 1) {
			result = (result * base)
		}
		base = (base * base)
		exp = (exp / 2)
	}
	return result
}

func example_1() {
	expect((myPow(2, 10) == 1024))
}

func example_2() {
	expect((myPow(2.1, 3) == 9.261000000000001))
}

func example_3() {
	expect((myPow(2, -2) == 0.25))
}

func main() {
	example_1()
	example_2()
	example_3()
}

