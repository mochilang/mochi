package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countNumbersWithUniqueDigits(n int) int {
	if (n == 0) {
		return 1
	}
	if (n > 10) {
		n = 10
	}
	var result int = 10
	var unique int = 9
	var available int = 9
	var i int = 2
	for (i <= n) {
		unique = (unique * available)
		result = (result + unique)
		available = (available - 1)
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect((countNumbersWithUniqueDigits(2) == 91))
}

func example_2() {
	expect((countNumbersWithUniqueDigits(0) == 1))
}

func three_digits() {
	expect((countNumbersWithUniqueDigits(3) == 739))
}

func up_to_ten_digits() {
	expect((countNumbersWithUniqueDigits(10) == 8877691))
}

func more_than_ten() {
	expect((countNumbersWithUniqueDigits(11) == 8877691))
}

func main() {
	example_1()
	example_2()
	three_digits()
	up_to_ten_digits()
	more_than_ten()
}

