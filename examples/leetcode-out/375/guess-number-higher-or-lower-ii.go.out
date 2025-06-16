package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func max(a int, b int) int {
	if (a > b) {
		return a
	}
	return b
}

func getMoneyAmount(n int) int {
	var dp [][]int = [][]int{}
	var i int = 0
	for (i <= n) {
		var row []int = []int{}
		var j int = 0
		for (j <= n) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		dp = append(append([][]int{}, dp...), [][]int{row}...)
		i = (i + 1)
	}
	var len int = 2
	for (len <= n) {
		var start int = 1
		for (start <= ((n - len) + 1)) {
			var end int = ((start + len) - 1)
			var best int = (n * n)
			var guess int = start
			for (guess <= end) {
				var left int = 0
				if ((guess - 1) >= start) {
					left = dp[start][(guess - 1)]
				}
				var right int = 0
				if ((guess + 1) <= end) {
					right = dp[(guess + 1)][end]
				}
				var cost int = guess
				if (left > right) {
					cost = (cost + left)
				} else {
					cost = (cost + right)
				}
				if (cost < best) {
					best = cost
				}
				guess = (guess + 1)
			}
			dp[start][end] = best
			start = (start + 1)
		}
		len = (len + 1)
	}
	return dp[1][n]
}

func example_1() {
	expect((getMoneyAmount(10) == 16))
}

func example_2() {
	expect((getMoneyAmount(1) == 0))
}

func example_3() {
	expect((getMoneyAmount(2) == 1))
}

func n_equals_3() {
	expect((getMoneyAmount(3) == 2))
}

func main() {
	example_1()
	example_2()
	example_3()
	n_equals_3()
}

