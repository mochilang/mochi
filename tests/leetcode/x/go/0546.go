package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type State struct {
	l int
	r int
	k int
}

func solve(boxes []int) int {
	memo := map[State]int{}
	var dp func(int, int, int) int
	dp = func(l, r, k int) int {
		if l > r {
			return 0
		}
		for l < r && boxes[r] == boxes[r-1] {
			r--
			k++
		}
		key := State{l, r, k}
		if v, ok := memo[key]; ok {
			return v
		}
		best := dp(l, r-1, 0) + (k+1)*(k+1)
		for i := l; i < r; i++ {
			if boxes[i] == boxes[r] {
				v := dp(l, i, k+1) + dp(i+1, r-1, 0)
				if v > best {
					best = v
				}
			}
		}
		memo[key] = best
		return best
	}
	return dp(0, len(boxes)-1, 0)
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		boxes := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &boxes[i])
		}
		out = append(out, fmt.Sprint(solve(boxes)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
