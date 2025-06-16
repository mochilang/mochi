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

func integerBreak(n int) int {
	if (n <= 2) {
		return 1
	}
	var dp []int = []int{}
	var i int = 0
	for (i <= n) {
		dp = append(append([]int{}, dp...), []int{0}...)
		i = (i + 1)
	}
	dp[1] = 1
	dp[2] = 1
	i = 3
	for (i <= n) {
		var j int = 1
		var best int = 0
		for (j < i) {
			var direct int = (j * ((i - j)))
			var broken int = (j * dp[(i - j)])
			var candidate int = max(direct, broken)
			if (candidate > best) {
				best = candidate
			}
			j = (j + 1)
		}
		dp[i] = best
		i = (i + 1)
	}
	return dp[n]
}

func example_1() {
	expect((integerBreak(2) == 1))
}

func example_2() {
	expect((integerBreak(10) == 36))
}

func n_equals_3() {
	expect((integerBreak(3) == 2))
}

func n_equals_4() {
	expect((integerBreak(4) == 4))
}

func main() {
	example_1()
	example_2()
	n_equals_3()
	n_equals_4()
}

