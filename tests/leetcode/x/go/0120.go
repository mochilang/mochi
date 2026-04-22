package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(tri [][]int) int {
	dp := append([]int{}, tri[len(tri)-1]...)
	for i := len(tri) - 2; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			if dp[j] < dp[j+1] {
				dp[j] = tri[i][j] + dp[j]
			} else {
				dp[j] = tri[i][j] + dp[j+1]
			}
		}
	}
	return dp[0]
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	nextInt := func() int { sc.Scan(); v, _ := strconv.Atoi(sc.Text()); return v }
	if !sc.Scan() { return }
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		rows := nextInt()
		tri := make([][]int, rows)
		for r := 1; r <= rows; r++ {
			tri[r-1] = make([]int, r)
			for j := 0; j < r; j++ { tri[r-1][j] = nextInt() }
		}
		if tc > 0 { fmt.Fprintln(w) }
		fmt.Fprint(w, solve(tri))
	}
}
