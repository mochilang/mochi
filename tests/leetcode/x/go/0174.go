package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(dungeon [][]int) int {
	cols := len(dungeon[0])
	const inf = int(1e18)
	dp := make([]int, cols+1)
	for i := range dp {
		dp[i] = inf
	}
	dp[cols-1] = 1
	for i := len(dungeon) - 1; i >= 0; i-- {
		for j := cols - 1; j >= 0; j-- {
			need := dp[j]
			if dp[j+1] < need {
				need = dp[j+1]
			}
			need -= dungeon[i][j]
			if need <= 1 {
				dp[j] = 1
			} else {
				dp[j] = need
			}
		}
	}
	return dp[0]
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	nextInt := func() int { sc.Scan(); v, _ := strconv.Atoi(sc.Text()); return v }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		rows := nextInt()
		cols := nextInt()
		dungeon := make([][]int, rows)
		for i := 0; i < rows; i++ {
			row := make([]int, cols)
			for j := 0; j < cols; j++ {
				row[j] = nextInt()
			}
			dungeon[i] = row
		}
		if tc > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, solve(dungeon))
	}
}
