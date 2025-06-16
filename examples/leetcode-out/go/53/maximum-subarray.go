package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxSubArray(nums []int) int {
	var n int = len(nums)
	var current int = nums[0]
	var best int = nums[0]
	for i := 1; i < n; i++ {
		var val int = nums[i]
		if ((current + val) > val) {
			current = (current + val)
		} else {
			current = val
		}
		if (current > best) {
			best = current
		}
	}
	return best
}

func example_1() {
	expect((maxSubArray([]int{-2, 1, -3, 4, -1, 2, 1, -5, 4}) == 6))
}

func example_2() {
	expect((maxSubArray([]int{1}) == 1))
}

func example_3() {
	expect((maxSubArray([]int{5, 4, -1, 7, 8}) == 23))
}

func all_negative() {
	expect((maxSubArray([]int{-3, -2, -5}) == (-2)))
}

func main() {
	example_1()
	example_2()
	example_3()
	all_negative()
}

