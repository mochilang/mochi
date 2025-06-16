package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numTrees(n int) int {
	var dp []int = []int{}
	var i int = 0
	for (i <= n) {
		dp = append(append([]int{}, dp...), []int{0}...)
		i = (i + 1)
	}
	dp[0] = 1
	if (n >= 1) {
		dp[1] = 1
	}
	i = 2
	for (i <= n) {
		var j int = 1
		for (j <= i) {
			dp[i] = (dp[i] + (dp[(j - 1)] * dp[(i - j)]))
			j = (j + 1)
		}
		i = (i + 1)
	}
	return dp[n]
}

func example_1() {
	expect((numTrees(3) == 5))
}

func example_2() {
	expect((numTrees(1) == 1))
}

func n___2() {
	expect((numTrees(2) == 2))
}

func n___5() {
	expect((numTrees(5) == 42))
}

func main() {
	example_1()
	example_2()
	n___2()
	n___5()
}

