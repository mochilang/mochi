package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func better(a, b string) string {
	if len(a) != len(b) {
		if len(a) < len(b) {
			return a
		}
		return b
	}
	if a < b {
		return a
	}
	return b
}

func repeatLen(s string) int {
	if len(s) == 0 {
		return 0
	}
	idx := strings.Index((s+s)[1:], s)
	if idx >= 0 {
		part := idx + 1
		if part < len(s) && len(s)%part == 0 {
			return part
		}
	}
	return len(s)
}

func encode(s string) string {
	n := len(s)
	dp := make([][]string, n)
	for i := range dp {
		dp[i] = make([]string, n)
	}
	for length := 1; length <= n; length++ {
		for i := 0; i+length <= n; i++ {
			j := i + length - 1
			sub := s[i : j+1]
			best := sub
			for k := i; k < j; k++ {
				best = better(best, dp[i][k]+dp[k+1][j])
			}
			part := repeatLen(sub)
			if part < length {
				best = better(best, fmt.Sprintf("%d[%s]", length/part, dp[i][i+part-1]))
			}
			dp[i][j] = best
		}
	}
	return dp[0][n-1]
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
		var s string
		fmt.Fscan(in, &s)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, encode(s))
	}
}
