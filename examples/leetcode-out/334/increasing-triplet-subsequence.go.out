package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func increasingTriplet(nums []int) bool {
	var first int = 2147483647
	var second int = 2147483647
	for _, n := range nums {
		if (n <= first) {
			first = n
		} else 		if (n <= second) {
			second = n
		} else {
			return true
		}
	}
	return false
}

func example_1() {
	expect((increasingTriplet([]int{1, 2, 3, 4, 5}) == true))
}

func example_2() {
	expect((increasingTriplet([]int{5, 4, 3, 2, 1}) == false))
}

func example_3() {
	expect((increasingTriplet([]int{2, 1, 5, 0, 4, 6}) == true))
}

func duplicate_numbers() {
	expect((increasingTriplet([]int{2, 2, 2, 2, 2}) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	duplicate_numbers()
}

