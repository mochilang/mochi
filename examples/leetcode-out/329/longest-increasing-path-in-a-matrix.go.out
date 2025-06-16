package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func longestIncreasingPath(matrix [][]int) int {
	var m int = len(matrix)
	if (m == 0) {
		return 0
	}
	var n int = len(matrix[0])
	var memo [][]int = [][]int{}
	var i int = 0
	for (i < m) {
		var row []int = []int{}
		var j int = 0
		for (j < n) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		memo = append(append([][]int{}, memo...), [][]int{row}...)
		i = (i + 1)
	}
	var dfs func(int, int) int
	dfs = func(x int, y int) int {
		var cached int = memo[x][y]
		if (cached != 0) {
			return cached
		}
		var val int = matrix[x][y]
		var best int = 1
		if (x > 0) {
			if (matrix[(x - 1)][y] > val) {
				var candidate int = (1 + dfs((x - 1), y))
				if (candidate > best) {
					best = candidate
				}
			}
		}
		if ((x + 1) < m) {
			if (matrix[(x + 1)][y] > val) {
				var candidate int = (1 + dfs((x + 1), y))
				if (candidate > best) {
					best = candidate
				}
			}
		}
		if (y > 0) {
			if (matrix[x][(y - 1)] > val) {
				var candidate int = (1 + dfs(x, (y - 1)))
				if (candidate > best) {
					best = candidate
				}
			}
		}
		if ((y + 1) < n) {
			if (matrix[x][(y + 1)] > val) {
				var candidate int = (1 + dfs(x, (y + 1)))
				if (candidate > best) {
					best = candidate
				}
			}
		}
		memo[x][y] = best
		return best
}
	var result int = 0
	i = 0
	for (i < m) {
		var j int = 0
		for (j < n) {
			var length int = dfs(i, j)
			if (length > result) {
				result = length
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return result
}

func example_1() {
	var matrix [][]int = [][]int{[]int{9, 9, 4}, []int{6, 6, 8}, []int{2, 1, 1}}
	_ = matrix
	expect((longestIncreasingPath(matrix) == 4))
}

func example_2() {
	var matrix [][]int = [][]int{[]int{3, 4, 5}, []int{3, 2, 6}, []int{2, 2, 1}}
	_ = matrix
	expect((longestIncreasingPath(matrix) == 4))
}

func single_cell() {
	expect((longestIncreasingPath([][]int{[]int{1}}) == 1))
}

func empty() {
	expect((longestIncreasingPath([][]int{}) == 0))
}

func main() {
	example_1()
	example_2()
	single_cell()
	empty()
}

