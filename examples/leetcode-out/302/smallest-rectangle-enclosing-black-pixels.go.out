package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func minArea(image [][]string, x int, y int) int {
	var rows int = len(image)
	var cols int = len(image[0])
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
	var minRow int = x
	var maxRow int = x
	var minCol int = y
	var maxCol int = y
	var dfs func(int, int) int
	dfs = func(i int, j int) int {
		if ((((i < 0) || (i >= rows)) || (j < 0)) || (j >= cols)) {
			return 0
		}
		if visited[i][j] {
			return 0
		}
		if (image[i][j] != "1") {
			return 0
		}
		visited[i][j] = true
		if (i < minRow) {
			minRow = i
		}
		if (i > maxRow) {
			maxRow = i
		}
		if (j < minCol) {
			minCol = j
		}
		if (j > maxCol) {
			maxCol = j
		}
		dfs((i + 1), j)
		dfs((i - 1), j)
		dfs(i, (j + 1))
		dfs(i, (j - 1))
		return 0
}
	dfs(x, y)
	var height int = ((maxRow - minRow) + 1)
	var width int = ((maxCol - minCol) + 1)
	return (height * width)
}

func example() {
	expect((minArea(example, 0, 2) == 6))
}

func single_pixel() {
	expect((minArea([][]string{[]string{"1"}}, 0, 0) == 1))
}

func all_ones() {
	expect((minArea([][]string{[]string{"1", "1"}, []string{"1", "1"}}, 1, 1) == 4))
}

var example [][]string = [][]string{[]string{"0", "0", "1", "0"}, []string{"0", "1", "1", "0"}, []string{"0", "1", "0", "0"}}
func main() {
	example()
	single_pixel()
	all_ones()
}

