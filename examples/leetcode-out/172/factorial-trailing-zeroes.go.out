package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func trailingZeroes(n int) int {
	var count int = 0
	var divisor int = 5
	for (divisor <= n) {
		count = (count + ((n / divisor)))
		divisor = (divisor * 5)
	}
	return count
}

func example_1() {
	expect((trailingZeroes(3) == 0))
}

func example_2() {
	expect((trailingZeroes(5) == 1))
}

func example_3() {
	expect((trailingZeroes(0) == 0))
}

func large_value() {
	expect((trailingZeroes(30) == 7))
}

func main() {
	example_1()
	example_2()
	example_3()
	large_value()
}

