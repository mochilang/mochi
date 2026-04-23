package main

import (
	"bufio"
	"fmt"
	"os"
)

func longestIncreasingPath(matrix [][]int) int {
	rows, cols := len(matrix), len(matrix[0])
	memo := make([][]int, rows)
	for i := range memo { memo[i] = make([]int, cols) }
	dirs := [][2]int{{1,0},{-1,0},{0,1},{0,-1}}
	var dfs func(int,int) int
	dfs = func(r, c int) int {
		if memo[r][c] != 0 { return memo[r][c] }
		best := 1
		for _, d := range dirs {
			nr, nc := r+d[0], c+d[1]
			if nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr][nc] > matrix[r][c] {
				if v := 1 + dfs(nr, nc); v > best { best = v }
			}
		}
		memo[r][c] = best
		return best
	}
	ans := 0
	for r := 0; r < rows; r++ { for c := 0; c < cols; c++ { if v := dfs(r,c); v > ans { ans = v } } }
	return ans
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout); defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil { return }
	for tc := 0; tc < t; tc++ {
		var rows, cols int; fmt.Fscan(in, &rows, &cols)
		m := make([][]int, rows)
		for r := range m { m[r] = make([]int, cols); for c := range m[r] { fmt.Fscan(in, &m[r][c]) } }
		if tc > 0 { fmt.Fprintln(out); fmt.Fprintln(out) }
		fmt.Fprint(out, longestIncreasingPath(m))
	}
}
