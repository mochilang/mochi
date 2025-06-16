package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func uniquePaths(m int, n int) int {
	var dp []int = []int{}
	var i int = 0
	for (i < n) {
		dp = append(append([]int{}, dp...), []int{1}...)
		i = (i + 1)
	}
	var row int = 1
	for (row < m) {
		var col int = 1
		for (col < n) {
			dp[col] = (dp[col] + dp[(col - 1)])
			col = (col + 1)
		}
		row = (row + 1)
	}
	return dp[(n - 1)]
}

func example_1() {
	expect((uniquePaths(3, 7) == 28))
}

func example_2() {
	expect((uniquePaths(3, 2) == 3))
}

func example_3() {
	expect((uniquePaths(7, 3) == 28))
}

func example_4() {
	expect((uniquePaths(3, 3) == 6))
}

func main() {
	example_1()
	example_2()
	example_3()
	example_4()
}

