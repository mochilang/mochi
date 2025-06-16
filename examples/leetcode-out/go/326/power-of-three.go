package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPowerOfThree(n int) bool {
	if (n <= 0) {
		return false
	}
	var value int = n
	for ((value % 3) == 0) {
		value = (value / 3)
	}
	return (value == 1)
}

func example_1() {
	expect((isPowerOfThree(27) == true))
}

func example_2() {
	expect((isPowerOfThree(0) == false))
}

func example_3() {
	expect((isPowerOfThree(9) == true))
}

func one_is_power() {
	expect((isPowerOfThree(1) == true))
}

func negative() {
	expect((isPowerOfThree(-3) == false))
}

func not_power() {
	expect((isPowerOfThree(45) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	one_is_power()
	negative()
	not_power()
}

