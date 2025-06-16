package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func exist(board [][]string, word string) bool {
	var m int = len(board)
	if (m == 0) {
		return false
	}
	var n int = len(board[0])
	var visited [][]bool = [][]bool{}
	var r int = 0
	for (r < m) {
		var row []bool = []bool{}
		var c int = 0
		for (c < n) {
			row = append(append([]bool{}, row...), []bool{false}...)
			c = (c + 1)
		}
		visited = append(append([][]bool{}, visited...), [][]bool{row}...)
		r = (r + 1)
	}
	var dfs func(int, int, int) bool
	dfs = func(r int, c int, idx int) bool {
		if (idx == len(word)) {
			return true
		}
		if ((((r < 0) || (r >= m)) || (c < 0)) || (c >= n)) {
			return false
		}
		if visited[r][c] {
			return false
		}
		if (board[r][c] != _indexString(word, idx)) {
			return false
		}
		visited[r][c] = true
		if (((dfs((r + 1), c, (idx + 1)) || dfs((r - 1), c, (idx + 1))) || dfs(r, (c + 1), (idx + 1))) || dfs(r, (c - 1), (idx + 1))) {
			visited[r][c] = false
			return true
		}
		visited[r][c] = false
		return false
}
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			if dfs(i, j, 0) {
				return true
			}
		}
	}
	return false
}

func example_1() {
	expect((exist(board, "ABCCED") == true))
}

func example_2() {
	expect((exist(board, "SEE") == true))
}

func example_3() {
	expect((exist(board, "ABCB") == false))
}

var board [][]string = [][]string{[]string{"A", "B", "C", "E"}, []string{"S", "F", "C", "S"}, []string{"A", "D", "E", "E"}}
func main() {
	example_1()
	example_2()
	example_3()
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

