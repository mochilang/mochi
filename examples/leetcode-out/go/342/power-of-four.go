package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPowerOfFour(n int) bool {
	if (n <= 0) {
		return false
	}
	var x int = n
	for ((x % 4) == 0) {
		x = (x / 4)
	}
	return (x == 1)
}

func example_1() {
	expect((isPowerOfFour(16) == true))
}

func example_2() {
	expect((isPowerOfFour(5) == false))
}

func power_of_two_but_not_four() {
	expect((isPowerOfFour(8) == false))
}

func largest_power_of_four_within_32_bit() {
	expect((isPowerOfFour(1073741824) == true))
}

func main() {
	example_1()
	example_2()
	power_of_two_but_not_four()
	largest_power_of_four_within_32_bit()
}

