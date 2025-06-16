package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isUgly(n int) bool {
	if (n <= 0) {
		return false
	}
	var num int = n
	for ((num % 2) == 0) {
		num = (num / 2)
	}
	for ((num % 3) == 0) {
		num = (num / 3)
	}
	for ((num % 5) == 0) {
		num = (num / 5)
	}
	return (num == 1)
}

func example_1() {
	expect((isUgly(6) == true))
}

func example_2() {
	expect((isUgly(1) == true))
}

func example_3() {
	expect((isUgly(14) == false))
}

func zero() {
	expect((isUgly(0) == false))
}

func negative() {
	expect((isUgly(-6) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	zero()
	negative()
}

