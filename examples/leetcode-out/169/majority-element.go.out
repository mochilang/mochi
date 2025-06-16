package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func majorityElement(nums []int) int {
	var count int = 0
	var candidate int = 0
	for _, n := range nums {
		if (count == 0) {
			candidate = n
			count = 1
		} else {
			if (n == candidate) {
				count = (count + 1)
			} else {
				count = (count - 1)
			}
		}
	}
	return candidate
}

func example_1() {
	expect((majorityElement([]int{3, 2, 3}) == 3))
}

func example_2() {
	expect((majorityElement([]int{2, 2, 1, 1, 1, 2, 2}) == 2))
}

func single_element() {
	expect((majorityElement([]int{1}) == 1))
}

func large_majority() {
	expect((majorityElement([]int{1, 1, 1, 2, 2}) == 1))
}

func main() {
	example_1()
	example_2()
	single_element()
	large_majority()
}

