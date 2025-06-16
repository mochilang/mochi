package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func coinChange(coins []int, amount int) int {
	var dp []int = []int{}
	var i int = 0
	for (i <= amount) {
		dp = append(append([]int{}, dp...), []int{(amount + 1)}...)
		i = (i + 1)
	}
	dp[0] = 0
	i = 1
	for (i <= amount) {
		var j int = 0
		for (j < len(coins)) {
			var c int = coins[j]
			if ((i - c) >= 0) {
				var candidate int = (dp[(i - c)] + 1)
				if (candidate < dp[i]) {
					dp[i] = candidate
				}
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	if (dp[amount] > amount) {
		return -1
	}
	return dp[amount]
}

func example_1() {
	expect((coinChange([]int{1, 2, 5}, 11) == 3))
}

func example_2() {
	expect((coinChange([]int{2}, 3) == (-1)))
}

func example_3() {
	expect((coinChange([]int{1}, 0) == 0))
}

func single_coin() {
	expect((coinChange([]int{2}, 4) == 2))
}

func mixed_coins() {
	expect((coinChange([]int{1, 2, 5}, 7) == 2))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_coin()
	mixed_coins()
}

