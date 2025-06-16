package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func search(nums []int, target int) bool {
	var left int = 0
	var right int = (len(nums) - 1)
	for (left <= right) {
		var mid int = (((left + right)) / 2)
		if (nums[mid] == target) {
			return true
		}
		if (nums[left] == nums[mid]) {
			left = (left + 1)
		} else 		if (nums[left] < nums[mid]) {
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
	return false
}

func example_1() {
	expect((search([]int{2, 5, 6, 0, 0, 1, 2}, 0) == true))
}

func example_2() {
	expect((search([]int{2, 5, 6, 0, 0, 1, 2}, 3) == false))
}

func all_duplicates() {
	expect((search([]int{1, 1, 1, 1, 1}, 2) == false))
}

func single_element() {
	expect((search([]int{1}, 1) == true))
}

func empty_array() {
	expect((search([]int{}, 5) == false))
}

func main() {
	example_1()
	example_2()
	all_duplicates()
	single_element()
	empty_array()
}

