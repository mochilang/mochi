package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func lengthOfLIS(nums []int) int {
	var n int = len(nums)
	if (n == 0) {
		return 0
	}
	var dp []int = []int{}
	var fill int = 0
	for (fill < n) {
		dp = append(append([]int{}, dp...), []int{1}...)
		fill = (fill + 1)
	}
	var i int = 1
	for (i < n) {
		var j int = 0
		for (j < i) {
			if (nums[i] > nums[j]) {
				var candidate int = (dp[j] + 1)
				if (candidate > dp[i]) {
					dp[i] = candidate
				}
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	var result int = dp[0]
	var k int = 1
	for (k < n) {
		if (dp[k] > result) {
			result = dp[k]
		}
		k = (k + 1)
	}
	return result
}

func example_1() {
	expect((lengthOfLIS([]int{10, 9, 2, 5, 3, 7, 101, 18}) == 4))
}

func example_2() {
	expect((lengthOfLIS([]int{0, 1, 0, 3, 2, 3}) == 4))
}

func example_3() {
	expect((lengthOfLIS([]int{7, 7, 7, 7, 7, 7, 7}) == 1))
}

func empty() {
	expect((lengthOfLIS([]int{}) == 0))
}

func single() {
	expect((lengthOfLIS([]int{5}) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty()
	single()
}

