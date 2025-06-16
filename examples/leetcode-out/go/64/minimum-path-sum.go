package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func minPathSum(grid [][]int) int {
	var rows int = len(grid)
	if (rows == 0) {
		return 0
	}
	var cols int = len(grid[0])
	var dp [][]int = [][]int{}
	var r int = 0
	for (r < rows) {
		var row []int = []int{}
		var c int = 0
		for (c < cols) {
			row = append(append([]int{}, row...), []int{0}...)
			c = (c + 1)
		}
		dp = append(append([][]int{}, dp...), [][]int{row}...)
		r = (r + 1)
	}
	dp[0][0] = grid[0][0]
	var c int = 1
	for (c < cols) {
		dp[0][c] = (dp[0][(c - 1)] + grid[0][c])
		c = (c + 1)
	}
	r = 1
	for (r < rows) {
		dp[r][0] = (dp[(r - 1)][0] + grid[r][0])
		var c int = 1
		for (c < cols) {
			var top int = dp[(r - 1)][c]
			var left int = dp[r][(c - 1)]
			if (top < left) {
				dp[r][c] = (top + grid[r][c])
			} else {
				dp[r][c] = (left + grid[r][c])
			}
			c = (c + 1)
		}
		r = (r + 1)
	}
	return dp[(rows - 1)][(cols - 1)]
}

func example_1() {
	expect((minPathSum([][]int{[]int{1, 3, 1}, []int{1, 5, 1}, []int{4, 2, 1}}) == 7))
}

func example_2() {
	expect((minPathSum([][]int{[]int{1, 2, 3}, []int{4, 5, 6}}) == 12))
}

func single_cell() {
	expect((minPathSum([][]int{[]int{1}}) == 1))
}

func main() {
	example_1()
	example_2()
	single_cell()
}

