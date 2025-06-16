package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func uniquePathsWithObstacles(grid [][]int) int {
	var m int = len(grid)
	if (m == 0) {
		return 0
	}
	var n int = len(grid[0])
	var dp [][]int = [][]int{}
	var i int = 0
	for (i < m) {
		var row []int = []int{}
		var j int = 0
		for (j < n) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		dp = append(append([][]int{}, dp...), [][]int{row}...)
		i = (i + 1)
	}
	if (grid[0][0] == 1) {
		return 0
	}
	dp[0][0] = 1
	i = 0
	for (i < m) {
		var j int = 0
		for (j < n) {
			if (grid[i][j] == 1) {
				dp[i][j] = 0
			} else {
				if (i > 0) {
					dp[i][j] = (dp[i][j] + dp[(i - 1)][j])
				}
				if (j > 0) {
					dp[i][j] = (dp[i][j] + dp[i][(j - 1)])
				}
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return dp[(m - 1)][(n - 1)]
}

func example_1() {
	expect((uniquePathsWithObstacles([][]int{[]int{0, 0, 0}, []int{0, 1, 0}, []int{0, 0, 0}}) == 2))
}

func example_2() {
	expect((uniquePathsWithObstacles([][]int{[]int{0, 1}, []int{0, 0}}) == 1))
}

func obstacle_at_start() {
	expect((uniquePathsWithObstacles([][]int{[]int{1}}) == 0))
}

func single_open_cell() {
	expect((uniquePathsWithObstacles([][]int{[]int{0}}) == 1))
}

func main() {
	example_1()
	example_2()
	obstacle_at_start()
	single_open_cell()
}

