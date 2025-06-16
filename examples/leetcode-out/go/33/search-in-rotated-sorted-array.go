package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func search(nums []int, target int) int {
	var left int = 0
	var right int = (len(nums) - 1)
	for (left <= right) {
		var mid int = (((left + right)) / 2)
		if (nums[mid] == target) {
			return mid
		}
		if (nums[left] <= nums[mid]) {
			if ((nums[left] <= target) && (target < nums[mid])) {
				right = (mid - 1)
			} else {
				left = (mid + 1)
			}
		} else {
			if ((nums[mid] < target) && (target <= nums[right])) {
				left = (mid + 1)
			} else {
				right = (mid - 1)
			}
		}
	}
	return -1
}

func example_1() {
	expect((search([]int{4, 5, 6, 7, 0, 1, 2}, 0) == 4))
}

func example_2() {
	expect((search([]int{4, 5, 6, 7, 0, 1, 2}, 3) == (-1)))
}

func example_3() {
	expect((search([]int{1}, 0) == (-1)))
}

func single_element_found() {
	expect((search([]int{1}, 1) == 0))
}

func two_elements() {
	expect((search([]int{3, 1}, 1) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_element_found()
	two_elements()
}

