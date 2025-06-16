package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func solveSudoku(board [][]string) [][]string {
	var isValid = func(row int, col int, ch string) bool {
		for i := 0; i < 9; i++ {
			if (board[row][i] == ch) {
				return false
			}
			if (board[i][col] == ch) {
				return false
			}
			var r int = ((((row / 3)) * 3) + (i / 3))
			var c int = ((((col / 3)) * 3) + (i % 3))
			if (board[r][c] == ch) {
				return false
			}
		}
		return true
}
	var dfs func(int, int) bool
	dfs = func(r int, c int) bool {
		if (r == 9) {
			return true
		}
		if (c == 9) {
			return dfs((r + 1), 0)
		}
		if (board[r][c] != ".") {
			return dfs(r, (c + 1))
		}
		for _, d := range []string{"1", "2", "3", "4", "5", "6", "7", "8", "9"} {
			if isValid(r, c, d) {
				board[r][c] = d
				if dfs(r, (c + 1)) {
					return true
				}
				board[r][c] = "."
			}
		}
		return false
}
	dfs(0, 0)
	return board
}

func solve() {
	expect(_equal(solvedBoard, solved))
}

var board [][]string = [][]string{[]string{"5", "3", ".", ".", "7", ".", ".", ".", "."}, []string{"6", ".", ".", "1", "9", "5", ".", ".", "."}, []string{".", "9", "8", ".", ".", ".", ".", "6", "."}, []string{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, []string{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, []string{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, []string{".", "6", ".", ".", ".", ".", "2", "8", "."}, []string{".", ".", ".", "4", "1", "9", ".", ".", "5"}, []string{".", ".", ".", ".", "8", ".", ".", "7", "9"}}
var solvedBoard [][]string = solveSudoku(board)
var solved [][]string = [][]string{[]string{"5", "3", "4", "6", "7", "8", "9", "1", "2"}, []string{"6", "7", "2", "1", "9", "5", "3", "4", "8"}, []string{"1", "9", "8", "3", "4", "2", "5", "6", "7"}, []string{"8", "5", "9", "7", "6", "1", "4", "2", "3"}, []string{"4", "2", "6", "8", "5", "3", "7", "9", "1"}, []string{"7", "1", "3", "9", "2", "4", "8", "5", "6"}, []string{"9", "6", "1", "5", "3", "7", "2", "8", "4"}, []string{"2", "8", "7", "4", "1", "9", "6", "3", "5"}, []string{"3", "4", "5", "2", "8", "6", "1", "7", "9"}}
func main() {
	solve()
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

