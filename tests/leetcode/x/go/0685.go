package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(edges [][]int) []int {
	n := len(edges)
	parentOf := make([]int, n+1)
	var candA, candB []int
	for _, e := range edges {
		u, v := e[0], e[1]
		if parentOf[v] == 0 {
			parentOf[v] = u
		} else {
			candA = []int{parentOf[v], v}
			candB = []int{u, v}
			break
		}
	}
	parent := make([]int, n+1)
	for i := range parent {
		parent[i] = i
	}
	var find func(int) int
	find = func(x int) int {
		for parent[x] != x {
			parent[x] = parent[parent[x]]
			x = parent[x]
		}
		return x
	}
	union := func(a, b int) bool {
		ra, rb := find(a), find(b)
		if ra == rb {
			return false
		}
		parent[rb] = ra
		return true
	}
	for _, e := range edges {
		if candB != nil && e[0] == candB[0] && e[1] == candB[1] {
			continue
		}
		if !union(e[0], e[1]) {
			if candA != nil {
				return candA
			}
			return e
		}
	}
	return candB
}

func fmtEdge(edge []int) string { return fmt.Sprintf("[%d,%d]", edge[0], edge[1]) }

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		edges := make([][]int, n)
		for i := 0; i < n; i++ {
			var u, v int
			fmt.Fscan(in, &u, &v)
			edges[i] = []int{u, v}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, fmtEdge(solve(edges)))
	}
	_ = strings.Builder{}
}
