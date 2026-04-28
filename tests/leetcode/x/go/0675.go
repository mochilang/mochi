package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type tree struct{ h, r, c int }
type node struct{ r, c, d int }

func bfs(forest [][]int, sr, sc, tr, tc int) int {
	if sr == tr && sc == tc {
		return 0
	}
	m, n := len(forest), len(forest[0])
	q := []node{{sr, sc, 0}}
	seen := make([][]bool, m)
	for i := range seen {
		seen[i] = make([]bool, n)
	}
	seen[sr][sc] = true
	for head := 0; head < len(q); head++ {
		cur := q[head]
		for _, dir := range [][2]int{{1, 0}, {-1, 0}, {0, 1}, {0, -1}} {
			nr, nc := cur.r+dir[0], cur.c+dir[1]
			if 0 <= nr && nr < m && 0 <= nc && nc < n && forest[nr][nc] != 0 && !seen[nr][nc] {
				if nr == tr && nc == tc {
					return cur.d + 1
				}
				seen[nr][nc] = true
				q = append(q, node{nr, nc, cur.d + 1})
			}
		}
	}
	return -1
}

func solve(forest [][]int) int {
	trees := make([]tree, 0)
	for i := range forest {
		for j, v := range forest[i] {
			if v > 1 {
				trees = append(trees, tree{v, i, j})
			}
		}
	}
	sort.Slice(trees, func(i, j int) bool { return trees[i].h < trees[j].h })
	sr, sc, total := 0, 0, 0
	for _, t := range trees {
		d := bfs(forest, sr, sc, t.r, t.c)
		if d < 0 {
			return -1
		}
		total += d
		sr, sc = t.r, t.c
	}
	return total
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var m, n int
		fmt.Fscan(in, &m, &n)
		forest := make([][]int, m)
		for i := 0; i < m; i++ {
			forest[i] = make([]int, n)
			for j := 0; j < n; j++ {
				fmt.Fscan(in, &forest[i][j])
			}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(forest))
	}
}
