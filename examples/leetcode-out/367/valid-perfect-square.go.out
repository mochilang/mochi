package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPerfectSquare(num int) bool {
	if (num < 1) {
		return false
	}
	var left int = 1
	var right int = num
	for (left <= right) {
		var mid int = (((left + right)) / 2)
		var square int = (mid * mid)
		if (square == num) {
			return true
		} else 		if (square < num) {
			left = (mid + 1)
		} else {
			right = (mid - 1)
		}
	}
	return false
}

func example_1() {
	expect((isPerfectSquare(16) == true))
}

func example_2() {
	expect((isPerfectSquare(14) == false))
}

func one() {
	expect((isPerfectSquare(1) == true))
}

func large_non_square() {
	expect((isPerfectSquare(2147483647) == false))
}

func main() {
	example_1()
	example_2()
	one()
	large_non_square()
}

