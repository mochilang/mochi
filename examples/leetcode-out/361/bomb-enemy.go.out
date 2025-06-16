package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxKilledEnemies(grid [][]string) int {
	var m int = len(grid)
	if (m == 0) {
		return 0
	}
	var n int = len(grid[0])
	var result int = 0
	var colHits []int = []int{}
	var i int = 0
	for (i < n) {
		colHits = append(append([]int{}, colHits...), []int{0}...)
		i = (i + 1)
	}
	var rowHits int = 0
	var r int = 0
	for (r < m) {
		var c int = 0
		for (c < n) {
			if ((c == 0) || (grid[r][(c - 1)] == "W")) {
				rowHits = 0
				var k int = c
				for (k < n) {
					if (grid[r][k] == "W") {
						break
					}
					if (grid[r][k] == "E") {
						rowHits = (rowHits + 1)
					}
					k = (k + 1)
				}
			}
			if ((r == 0) || (grid[(r - 1)][c] == "W")) {
				colHits[c] = 0
				var k int = r
				for (k < m) {
					if (grid[k][c] == "W") {
						break
					}
					if (grid[k][c] == "E") {
						colHits[c] = (colHits[c] + 1)
					}
					k = (k + 1)
				}
			}
			if (grid[r][c] == "0") {
				var total int = (rowHits + colHits[c])
				if (total > result) {
					result = total
				}
			}
			c = (c + 1)
		}
		r = (r + 1)
	}
	return result
}

func example_1() {
	var grid [][]string = [][]string{[]string{"0", "E", "0", "0"}, []string{"E", "0", "W", "E"}, []string{"0", "E", "0", "0"}}
	_ = grid
	expect((maxKilledEnemies(grid) == 3))
}

func empty_grid() {
	expect((maxKilledEnemies([][]string{}) == 0))
}

func all_walls() {
	expect((maxKilledEnemies([][]string{[]string{"W", "W"}, []string{"W", "W"}}) == 0))
}

func main() {
	example_1()
	empty_grid()
	all_walls()
}

