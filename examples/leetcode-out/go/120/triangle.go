package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func minimumTotal(triangle [][]int) int {
	var n int = len(triangle)
	if (n == 0) {
		return 0
	}
	var dp []int = triangle[(n - 1)]
	var i int = (n - 2)
	for (i >= 0) {
		var j int = 0
		for (j <= i) {
			var left int = dp[j]
			var right int = dp[(j + 1)]
			if (left < right) {
				dp[j] = (triangle[i][j] + left)
			} else {
				dp[j] = (triangle[i][j] + right)
			}
			j = (j + 1)
		}
		i = (i - 1)
	}
	return dp[0]
}

func example_1() {
	expect((minimumTotal([][]int{[]int{2}, []int{3, 4}, []int{6, 5, 7}, []int{4, 1, 8, 3}}) == 11))
}

func example_2() {
	expect((minimumTotal([][]int{[]int{-10}}) == (-10)))
}

func single_level() {
	expect((minimumTotal([][]int{[]int{1}}) == 1))
}

func main() {
	example_1()
	example_2()
	single_level()
}

