package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findMin(nums []int) int {
	var left int = 0
	var right int = (len(nums) - 1)
	for (left < right) {
		var mid int = (left + (((right - left)) / 2))
		if (nums[mid] > nums[right]) {
			left = (mid + 1)
		} else {
			right = mid
		}
	}
	return nums[left]
}

func example_1() {
	expect((findMin([]int{3, 4, 5, 1, 2}) == 1))
}

func example_2() {
	expect((findMin([]int{4, 5, 6, 7, 0, 1, 2}) == 0))
}

func example_3() {
	expect((findMin([]int{11, 13, 15, 17}) == 11))
}

func single_element() {
	expect((findMin([]int{5}) == 5))
}

func two_elements() {
	expect((findMin([]int{2, 1}) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_element()
	two_elements()
}

