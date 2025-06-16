package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numSquares(n int) int {
	if (n <= 0) {
		return 0
	}
	var dp []int = []int{}
	var i int = 0
	for (i <= n) {
		dp = append(append([]int{}, dp...), []int{i}...)
		i = (i + 1)
	}
	i = 1
	for (i <= n) {
		var j int = 1
		dp[i] = i
		for ((j * j) <= i) {
			var candidate int = (dp[(i - (j * j))] + 1)
			if (candidate < dp[i]) {
				dp[i] = candidate
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return dp[n]
}

func example_1() {
	expect((numSquares(12) == 3))
}

func example_2() {
	expect((numSquares(13) == 2))
}

func example_3() {
	expect((numSquares(1) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
}

