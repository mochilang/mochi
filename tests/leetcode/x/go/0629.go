package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const mod629 = 1_000_000_007

func solve(n int, k int) int {
	dp := make([]int, k+1)
	dp[0] = 1
	for num := 1; num <= n; num++ {
		ndp := make([]int, k+1)
		window := 0
		for inv := 0; inv <= k; inv++ {
			window += dp[inv]
			if window >= mod629 {
				window -= mod629
			}
			if inv >= num {
				window -= dp[inv-num]
				if window < 0 {
					window += mod629
				}
			}
			ndp[inv] = window
		}
		dp = ndp
	}
	return dp[k]
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for i := 0; i < t; i++ {
		var n, k int
		fmt.Fscan(in, &n, &k)
		out = append(out, fmt.Sprint(solve(n, k)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
