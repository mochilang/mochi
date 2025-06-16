package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type TicTacToe struct {
	N int `json:"n"`
	Rows []int `json:"rows"`
	Cols []int `json:"cols"`
	Diag int `json:"diag"`
	Anti int `json:"anti"`
}

type MoveResult struct {
	Board TicTacToe `json:"board"`
	Winner int `json:"winner"`
}

func newGame(n int) TicTacToe {
	var rs []int = []int{}
	var cs []int = []int{}
	for i := 0; i < n; i++ {
		rs = append(append([]int{}, rs...), []int{0}...)
		cs = append(append([]int{}, cs...), []int{0}...)
	}
	return TicTacToe{N: n, Rows: rs, Cols: cs, Diag: 0, Anti: 0}
}

func setAt(xs []int, idx int, val int) []int {
	return append(append([]int{}, append(append([]int{}, xs[0:idx]...), []int{val}...)...), xs[(idx + 1):len(xs)]...)
}

func move(game TicTacToe, row int, col int, player int) MoveResult {
	var add int = 0
	if (player == 1) {
		add = 1
	} else {
		add = -1
	}
	var r int = (game.Rows[row] + add)
	var c int = (game.Cols[col] + add)
	var d int = game.Diag
	var a int = game.Anti
	if (row == col) {
		d = (d + add)
	}
	if ((row + col) == (game.N - 1)) {
		a = (a + add)
	}
	var newRows []int = setAt(game.Rows, row, r)
	var newCols []int = setAt(game.Cols, col, c)
	var newBoard TicTacToe = TicTacToe{N: game.N, Rows: newRows, Cols: newCols, Diag: d, Anti: a}
	var target int = game.N
	if ((((((((r == target) || (r == (-target))) || (c == target)) || (c == (-target))) || (d == target)) || (d == (-target))) || (a == target)) || (a == (-target))) {
		return MoveResult{Board: newBoard, Winner: player}
	}
	return MoveResult{Board: newBoard, Winner: 0}
}

func example() {
	var g TicTacToe = newGame(3)
	var m1 MoveResult = move(g, 0, 0, 1)
	_ = m1
	g = m1.Board
	expect((m1.Winner == 0))
	var m2 MoveResult = move(g, 0, 2, 2)
	_ = m2
	g = m2.Board
	expect((m2.Winner == 0))
	var m3 MoveResult = move(g, 2, 2, 1)
	_ = m3
	g = m3.Board
	expect((m3.Winner == 0))
	var m4 MoveResult = move(g, 1, 1, 2)
	_ = m4
	g = m4.Board
	expect((m4.Winner == 0))
	var m5 MoveResult = move(g, 2, 0, 1)
	_ = m5
	g = m5.Board
	expect((m5.Winner == 0))
	var m6 MoveResult = move(g, 1, 0, 2)
	_ = m6
	g = m6.Board
	expect((m6.Winner == 0))
	var m7 MoveResult = move(g, 2, 1, 1)
	_ = m7
	g = m7.Board
	expect((m7.Winner == 1))
}

func row_win() {
	var g TicTacToe = newGame(3)
	var r1 MoveResult = move(g, 0, 0, 2)
	_ = r1
	g = r1.Board
	var r2 MoveResult = move(g, 0, 1, 2)
	_ = r2
	g = r2.Board
	var res MoveResult = move(g, 0, 2, 2)
	_ = res
	g = res.Board
	expect((res.Winner == 2))
}

func column_win() {
	var g TicTacToe = newGame(3)
	var r1 MoveResult = move(g, 0, 1, 1)
	_ = r1
	g = r1.Board
	var r2 MoveResult = move(g, 1, 1, 1)
	_ = r2
	g = r2.Board
	var res MoveResult = move(g, 2, 1, 1)
	_ = res
	g = res.Board
	expect((res.Winner == 1))
}

func diagonal_win() {
	var g TicTacToe = newGame(3)
	var r1 MoveResult = move(g, 0, 0, 1)
	_ = r1
	g = r1.Board
	var r2 MoveResult = move(g, 1, 1, 1)
	_ = r2
	g = r2.Board
	var res MoveResult = move(g, 2, 2, 1)
	_ = res
	g = res.Board
	expect((res.Winner == 1))
}

func anti_diagonal_win() {
	var g TicTacToe = newGame(3)
	var r1 MoveResult = move(g, 0, 2, 2)
	_ = r1
	g = r1.Board
	var r2 MoveResult = move(g, 1, 1, 2)
	_ = r2
	g = r2.Board
	var res MoveResult = move(g, 2, 0, 2)
	_ = res
	g = res.Board
	expect((res.Winner == 2))
}

func main() {
	example()
	row_win()
	column_win()
	diagonal_win()
	anti_diagonal_win()
}

