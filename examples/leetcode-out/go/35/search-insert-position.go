package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func searchInsert(nums []int, target int) int {
	var left int = 0
	var right int = len(nums)
	for (left < right) {
		var mid int = (((left + right)) / 2)
		var value int = nums[mid]
		if (value < target) {
			left = (mid + 1)
		} else {
			right = mid
		}
	}
	return left
}

func example_1() {
	expect((searchInsert([]int{1, 3, 5, 6}, 5) == 2))
}

func example_2() {
	expect((searchInsert([]int{1, 3, 5, 6}, 2) == 1))
}

func example_3() {
	expect((searchInsert([]int{1, 3, 5, 6}, 7) == 4))
}

func example_4() {
	expect((searchInsert([]int{1, 3, 5, 6}, 0) == 0))
}

func single_element_greater() {
	expect((searchInsert([]int{2}, 1) == 0))
}

func single_element_smaller() {
	expect((searchInsert([]int{2}, 3) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	example_4()
	single_element_greater()
	single_element_smaller()
}

