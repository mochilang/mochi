package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numIslands(grid [][]string) int {
	var rows int = len(grid)
	if (rows == 0) {
		return 0
	}
	var cols int = len(grid[0])
	var visited [][]bool = [][]bool{}
	var r int = 0
	for (r < rows) {
		var row []bool = []bool{}
		var c int = 0
		for (c < cols) {
			row = append(append([]bool{}, row...), []bool{false}...)
			c = (c + 1)
		}
		visited = append(append([][]bool{}, visited...), [][]bool{row}...)
		r = (r + 1)
	}
	var dfs func(int, int) int
	dfs = func(i int, j int) int {
		if ((((i < 0) || (i >= rows)) || (j < 0)) || (j >= cols)) {
			return 0
		}
		if visited[i][j] {
			return 0
		}
		if (grid[i][j] != "1") {
			return 0
		}
		visited[i][j] = true
		dfs((i + 1), j)
		dfs((i - 1), j)
		dfs(i, (j + 1))
		dfs(i, (j - 1))
		return 0
}
	var count int = 0
	r = 0
	for (r < rows) {
		var c int = 0
		for (c < cols) {
			if (grid[r][c] == "1") {
				if !(visited[r][c]) {
					dfs(r, c)
					count = (count + 1)
				}
			}
			c = (c + 1)
		}
		r = (r + 1)
	}
	return count
}

func example_1() {
	expect((numIslands(grid1) == 1))
}

func example_2() {
	expect((numIslands(grid2) == 3))
}

func empty_grid() {
	expect((numIslands([][]string{}) == 0))
}

func all_water() {
	expect((numIslands([][]string{[]string{"0", "0"}, []string{"0", "0"}}) == 0))
}

func single_island() {
	expect((numIslands([][]string{[]string{"1"}}) == 1))
}

var grid1 [][]string = [][]string{[]string{"1", "1", "1", "1", "0"}, []string{"1", "1", "0", "1", "0"}, []string{"1", "1", "0", "0", "0"}, []string{"0", "0", "0", "0", "0"}}
var grid2 [][]string = [][]string{[]string{"1", "1", "0", "0", "0"}, []string{"1", "1", "0", "0", "0"}, []string{"0", "0", "1", "0", "0"}, []string{"0", "0", "0", "1", "1"}}
func main() {
	example_1()
	example_2()
	empty_grid()
	all_water()
	single_island()
}

