package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type State struct {
	d, r, c int
	p       string
}

type PQ []State

func (pq PQ) Len() int { return len(pq) }
func (pq PQ) Less(i, j int) bool {
	if pq[i].d != pq[j].d {
		return pq[i].d < pq[j].d
	}
	return pq[i].p < pq[j].p
}
func (pq PQ) Swap(i, j int) { pq[i], pq[j] = pq[j], pq[i] }
func (pq *PQ) Push(x any)   { *pq = append(*pq, x.(State)) }
func (pq *PQ) Pop() any {
	old := *pq
	x := old[len(old)-1]
	*pq = old[:len(old)-1]
	return x
}

func solve(maze [][]int, ball, hole [2]int) string {
	m, n := len(maze), len(maze[0])
	const inf = int(1e9)
	dist := make([][]int, m)
	path := make([][]string, m)
	for i := 0; i < m; i++ {
		dist[i] = make([]int, n)
		path[i] = make([]string, n)
		for j := 0; j < n; j++ {
			dist[i][j] = inf
		}
	}
	pq := &PQ{{0, ball[0], ball[1], ""}}
	heap.Init(pq)
	dist[ball[0]][ball[1]] = 0
	path[ball[0]][ball[1]] = ""
	dr := []int{1, 0, 0, -1}
	dc := []int{0, -1, 1, 0}
	ch := []byte{'d', 'l', 'r', 'u'}
	for pq.Len() > 0 {
		cur := heap.Pop(pq).(State)
		if cur.r == hole[0] && cur.c == hole[1] {
			return cur.p
		}
		if cur.d != dist[cur.r][cur.c] || cur.p != path[cur.r][cur.c] {
			continue
		}
		for k := 0; k < 4; k++ {
			nr, nc, nd := cur.r, cur.c, cur.d
            for nr+dr[k] >= 0 && nr+dr[k] < m && nc+dc[k] >= 0 && nc+dc[k] < n && maze[nr+dr[k]][nc+dc[k]] == 0 {
                nr += dr[k]
                nc += dc[k]
                nd++
                if nr == hole[0] && nc == hole[1] {
                    break
                }
            }
            if nr == cur.r && nc == cur.c {
                continue
            }
            np := cur.p + string(ch[k])
            if nd < dist[nr][nc] || (nd == dist[nr][nc] && (path[nr][nc] == "" || np < path[nr][nc])) {
                dist[nr][nc] = nd
                path[nr][nc] = np
                heap.Push(pq, State{nd, nr, nc, np})
			}
		}
	}
	return "impossible"
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
		maze := make([][]int, m)
		for i := 0; i < m; i++ {
			maze[i] = make([]int, n)
			for j := 0; j < n; j++ {
				fmt.Fscan(in, &maze[i][j])
			}
		}
		var ball, hole [2]int
		fmt.Fscan(in, &ball[0], &ball[1], &hole[0], &hole[1])
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(maze, ball, hole))
	}
}
