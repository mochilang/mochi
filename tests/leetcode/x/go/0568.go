package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(flights [][]int, days [][]int) int {
	n := len(flights)
	weeks := len(days[0])
	const neg = -1 << 60
	dp := make([]int, n)
	for i := range dp {
		dp[i] = neg
	}
	dp[0] = 0
	for week := 0; week < weeks; week++ {
		ndp := make([]int, n)
		for i := range ndp {
			ndp[i] = neg
		}
		for city := 0; city < n; city++ {
			if dp[city] == neg {
				continue
			}
			for nxt := 0; nxt < n; nxt++ {
				if city == nxt || flights[city][nxt] == 1 {
					val := dp[city] + days[nxt][week]
					if val > ndp[nxt] {
						ndp[nxt] = val
					}
				}
			}
		}
		dp = ndp
	}
	best := dp[0]
	for _, v := range dp {
		if v > best {
			best = v
		}
	}
	return best
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		var n, w int
		fmt.Fscan(in, &n, &w)
		flights := make([][]int, n)
		for i := 0; i < n; i++ {
			flights[i] = make([]int, n)
			for j := 0; j < n; j++ {
				fmt.Fscan(in, &flights[i][j])
			}
		}
		days := make([][]int, n)
		for i := 0; i < n; i++ {
			days[i] = make([]int, w)
			for j := 0; j < w; j++ {
				fmt.Fscan(in, &days[i][j])
			}
		}
		out = append(out, fmt.Sprint(solve(flights, days)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
