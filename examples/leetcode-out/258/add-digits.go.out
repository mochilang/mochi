package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func addDigits(num int) int {
	var n int = num
	for (n >= 10) {
		var sum int = 0
		for (n > 0) {
			sum = (sum + (n % 10))
			n = (n / 10)
		}
		n = sum
	}
	return n
}

func example_1() {
	expect((addDigits(38) == 2))
}

func example_2() {
	expect((addDigits(0) == 0))
}

func example_3() {
	expect((addDigits(99) == 9))
}

func single_digit() {
	expect((addDigits(7) == 7))
}

func large_number() {
	expect((addDigits(123456) == 3))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_digit()
	large_number()
}

