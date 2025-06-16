package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func combinationSum4(nums []int, target int) int {
	var dp []int = []int{1}
	var t int = 1
	for (t <= target) {
		var count int = 0
		for _, num := range nums {
			if (num <= t) {
				count = (count + dp[(t - num)])
			}
		}
		dp = append(append([]int{}, dp...), []int{count}...)
		t = (t + 1)
	}
	return dp[target]
}

func example_1() {
	expect((combinationSum4([]int{1, 2, 3}, 4) == 7))
}

func example_2() {
	expect((combinationSum4([]int{9}, 3) == 0))
}

func main() {
	example_1()
	example_2()
}

