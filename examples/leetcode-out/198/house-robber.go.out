package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func rob(nums []int) int {
	var n int = len(nums)
	if (n == 0) {
		return 0
	}
	if (n == 1) {
		return nums[0]
	}
	var prev2 int = nums[0]
	var prev1 int = nums[0]
	if (nums[1] > prev1) {
		prev1 = nums[1]
	}
	var i int = 2
	for (i < n) {
		var take int = (prev2 + nums[i])
		var best int = prev1
		if (take > best) {
			best = take
		}
		prev2 = prev1
		prev1 = best
		i = (i + 1)
	}
	return prev1
}

func example_1() {
	expect((rob([]int{1, 2, 3, 1}) == 4))
}

func example_2() {
	expect((rob([]int{2, 7, 9, 3, 1}) == 12))
}

func empty() {
	expect((rob([]int{}) == 0))
}

func single_house() {
	expect((rob([]int{5}) == 5))
}

func two_houses() {
	expect((rob([]int{2, 1}) == 2))
}

func main() {
	example_1()
	example_2()
	empty()
	single_house()
	two_houses()
}

