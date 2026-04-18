package main

import (
	"bufio"
	"fmt"
	"os"
)

func solve(s1, s2, s3 string) bool {
	m, n := len(s1), len(s2)
	if m+n != len(s3) {
		return false
	}
	dp := make([][]bool, m+1)
	for i := range dp {
		dp[i] = make([]bool, n+1)
	}
	dp[0][0] = true
	for i := 0; i <= m; i++ {
		for j := 0; j <= n; j++ {
			if i > 0 && dp[i-1][j] && s1[i-1] == s3[i+j-1] {
				dp[i][j] = true
			}
			if j > 0 && dp[i][j-1] && s2[j-1] == s3[i+j-1] {
				dp[i][j] = true
			}
		}
	}
	return dp[m][n]
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	lines := []string{}
	for sc.Scan() { lines = append(lines, sc.Text()) }
	if len(lines) == 0 { return }
	var t int
	fmt.Sscanf(lines[0], "%d", &t)
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for i := 0; i < t; i++ {
		if i > 0 { fmt.Fprintln(w) }
		if solve(lines[1+3*i], lines[2+3*i], lines[3+3*i]) { fmt.Fprint(w, "true") } else { fmt.Fprint(w, "false") }
	}
}
