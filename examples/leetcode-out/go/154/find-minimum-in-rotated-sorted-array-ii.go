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
		} else 		if (nums[mid] < nums[right]) {
			right = mid
		} else {
			right = (right - 1)
		}
	}
	return nums[left]
}

func example_1() {
	expect((findMin([]int{1, 3, 5}) == 1))
}

func example_2() {
	expect((findMin([]int{2, 2, 2, 0, 1}) == 0))
}

func already_sorted() {
	expect((findMin([]int{1, 2, 3, 4}) == 1))
}

func rotated_with_duplicates() {
	expect((findMin([]int{3, 4, 5, 1, 2, 2}) == 1))
}

func main() {
	example_1()
	example_2()
	already_sorted()
	rotated_with_duplicates()
}

