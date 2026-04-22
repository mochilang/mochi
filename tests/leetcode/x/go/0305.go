package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func solve(m, n int, positions [][2]int) []int {
	parent := map[int]int{}
	rank := map[int]int{}
	count := 0
	ans := make([]int, 0, len(positions))

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
		if rank[ra] < rank[rb] {
			ra, rb = rb, ra
		}
		parent[rb] = ra
		if rank[ra] == rank[rb] {
			rank[ra]++
		}
		return true
	}

	for _, p := range positions {
		r, c := p[0], p[1]
		idx := r*n + c
		if _, ok := parent[idx]; ok {
			ans = append(ans, count)
			continue
		}
		parent[idx] = idx
		rank[idx] = 0
		count++
		dirs := [][2]int{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
		for _, d := range dirs {
			nr, nc := r+d[0], c+d[1]
			if nr >= 0 && nr < m && nc >= 0 && nc < n {
				nei := nr*n + nc
				if _, ok := parent[nei]; ok && union(idx, nei) {
					count--
				}
			}
		}
		ans = append(ans, count)
	}
	return ans
}

func fmtList(a []int) string {
	parts := make([]string, len(a))
	for i, v := range a {
		parts[i] = strconv.Itoa(v)
	}
	return "[" + strings.Join(parts, ",") + "]"
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
		m, n, k := nextInt(), nextInt(), nextInt()
		positions := make([][2]int, k)
		for i := 0; i < k; i++ {
			positions[i] = [2]int{nextInt(), nextInt()}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, fmtList(solve(m, n, positions)))
	}
}
