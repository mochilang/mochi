package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type state struct{ r, c, d int }

func shortestDistance(grid [][]int) int {
	rows, cols := len(grid), len(grid[0])
	dist := make([][]int, rows)
	reach := make([][]int, rows)
	for i := range dist {
		dist[i] = make([]int, cols)
		reach[i] = make([]int, cols)
	}
	buildings := 0
	for sr := 0; sr < rows; sr++ {
		for sc := 0; sc < cols; sc++ {
			if grid[sr][sc] != 1 {
				continue
			}
			buildings++
			seen := make([][]bool, rows)
			for i := range seen {
				seen[i] = make([]bool, cols)
			}
			seen[sr][sc] = true
			q := []state{{sr, sc, 0}}
			for head := 0; head < len(q); head++ {
				cur := q[head]
				for _, d := range [][2]int{{1, 0}, {-1, 0}, {0, 1}, {0, -1}} {
					nr, nc := cur.r+d[0], cur.c+d[1]
					if nr >= 0 && nr < rows && nc >= 0 && nc < cols && !seen[nr][nc] {
						seen[nr][nc] = true
						if grid[nr][nc] == 0 {
							dist[nr][nc] += cur.d + 1
							reach[nr][nc]++
							q = append(q, state{nr, nc, cur.d + 1})
						}
					}
				}
			}
		}
	}
	ans := -1
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 0 && reach[r][c] == buildings {
				if ans == -1 || dist[r][c] < ans {
					ans = dist[r][c]
				}
			}
		}
	}
	return ans
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	in.Split(bufio.ScanWords)
	nextInt := func() int {
		in.Scan()
		v, _ := strconv.Atoi(in.Text())
		return v
	}
	if !in.Scan() {
		return
	}
	t, _ := strconv.Atoi(in.Text())
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		r, c := nextInt(), nextInt()
		grid := make([][]int, r)
		for i := 0; i < r; i++ {
			grid[i] = make([]int, c)
			for j := 0; j < c; j++ {
				grid[i][j] = nextInt()
			}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, shortestDistance(grid))
	}
}
