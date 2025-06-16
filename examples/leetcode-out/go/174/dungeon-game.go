package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func calculateMinimumHP(dungeon [][]int) int {
	var m int = len(dungeon)
	if (m == 0) {
		return 1
	}
	var n int = len(dungeon[0])
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
	var need int = (1 - dungeon[(m - 1)][(n - 1)])
	if (need <= 0) {
		need = 1
	}
	dp[(m - 1)][(n - 1)] = need
	var col int = (n - 2)
	for (col >= 0) {
		need = (dp[(m - 1)][(col + 1)] - dungeon[(m - 1)][col])
		if (need <= 0) {
			need = 1
		}
		dp[(m - 1)][col] = need
		col = (col - 1)
	}
	var rowi int = (m - 2)
	for (rowi >= 0) {
		need = (dp[(rowi + 1)][(n - 1)] - dungeon[rowi][(n - 1)])
		if (need <= 0) {
			need = 1
		}
		dp[rowi][(n - 1)] = need
		rowi = (rowi - 1)
	}
	i = (m - 2)
	for (i >= 0) {
		col = (n - 2)
		for (col >= 0) {
			var best int = dp[(i + 1)][col]
			if (dp[i][(col + 1)] < best) {
				best = dp[i][(col + 1)]
			}
			need = (best - dungeon[i][col])
			if (need <= 0) {
				need = 1
			}
			dp[i][col] = need
			col = (col - 1)
		}
		i = (i - 1)
	}
	return dp[0][0]
}

func example_1() {
	var board [][]int = [][]int{[]int{-2, -3, 3}, []int{-5, -10, 1}, []int{10, 30, -5}}
	_ = board
	expect((calculateMinimumHP(board) == 7))
}

func single_cell_positive() {
	expect((calculateMinimumHP([][]int{[]int{5}}) == 1))
}

func single_cell_negative() {
	expect((calculateMinimumHP([][]int{[]int{-5}}) == 6))
}

func two_by_two() {
	var board [][]int = [][]int{[]int{1, -2, 3}, []int{2, -2, -2}}
	_ = board
	expect((calculateMinimumHP(board) == 2))
}

func main() {
	example_1()
	single_cell_positive()
	single_cell_negative()
	two_by_two()
}

