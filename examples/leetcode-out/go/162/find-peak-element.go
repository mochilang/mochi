package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findPeakElement(nums []int) int {
	var n int = len(nums)
	var left int = 0
	var right int = (n - 1)
	for (left < right) {
		var mid int = (((left + right)) / 2)
		if (nums[mid] > nums[(mid + 1)]) {
			right = mid
		} else {
			left = (mid + 1)
		}
	}
	return left
}

func example_1() {
	expect((findPeakElement([]int{1, 2, 3, 1}) == 2))
}

func example_2() {
	var idx int = findPeakElement([]int{1, 2, 1, 3, 5, 6, 4})
	_ = idx
	expect(((idx == 1) || (idx == 5)))
}

func main() {
	example_1()
	example_2()
}

