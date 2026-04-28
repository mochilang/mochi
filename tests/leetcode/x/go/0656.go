package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"
)

func solve(coins []int, maxJump int) []int {
	n := len(coins)
	dp := make([]int, n)
	path := make([][]int, n)
	for i := range dp {
		dp[i] = math.MaxInt / 4
	}
	if coins[n-1] == -1 {
		return []int{}
	}
	dp[n-1] = coins[n-1]
	path[n-1] = []int{n}
	for i := n - 2; i >= 0; i-- {
		if coins[i] == -1 {
			continue
		}
		bestCost := math.MaxInt / 4
		var bestPath []int
		for j := i + 1; j < n && j <= i+maxJump; j++ {
			if dp[j] >= math.MaxInt/8 || path[j] == nil {
				continue
			}
			cost := coins[i] + dp[j]
			cand := append([]int{i + 1}, path[j]...)
			if cost < bestCost || cost == bestCost && lessPath(cand, bestPath) {
				bestCost = cost
				bestPath = cand
			}
		}
		dp[i] = bestCost
		path[i] = bestPath
	}
	if path[0] == nil {
		return []int{}
	}
	return path[0]
}

func lessPath(a, b []int) bool {
	if b == nil {
		return true
	}
	for i := 0; i < len(a) && i < len(b); i++ {
		if a[i] != b[i] {
			return a[i] < b[i]
		}
	}
	return len(a) < len(b)
}

func fmtPath(path []int) string {
	parts := make([]string, len(path))
	for i, x := range path {
		parts[i] = fmt.Sprint(x)
	}
	return "[" + strings.Join(parts, ",") + "]"
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
		var n, maxJump int
		fmt.Fscan(in, &n, &maxJump)
		coins := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &coins[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, fmtPath(solve(coins, maxJump)))
	}
}
