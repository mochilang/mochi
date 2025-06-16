package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func guessNumber(n int, pick int) int {
	var low int = 1
	var high int = n
	for (low <= high) {
		var mid int = (((low + high)) / 2)
		if (mid == pick) {
			return mid
		} else 		if (mid > pick) {
			high = (mid - 1)
		} else {
			low = (mid + 1)
		}
	}
	return -1
}

func example_1() {
	expect((guessNumber(10, 6) == 6))
}

func example_2() {
	expect((guessNumber(1, 1) == 1))
}

func first_number() {
	expect((guessNumber(5, 1) == 1))
}

func last_number() {
	expect((guessNumber(5, 5) == 5))
}

func main() {
	example_1()
	example_2()
	first_number()
	last_number()
}

