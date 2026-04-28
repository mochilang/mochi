package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func solve(bulbs []int, k int) int {
	n := len(bulbs)
	days := make([]int, n)
	for day, bulb := range bulbs {
		days[bulb-1] = day + 1
	}
	ans := math.MaxInt / 4
	left, right := 0, k+1
	for right < n {
		valid := true
		for i := left + 1; i < right; i++ {
			if days[i] < max(days[left], days[right]) {
				left = i
				right = i + k + 1
				valid = false
				break
			}
		}
		if valid {
			ans = min(ans, max(days[left], days[right]))
			left = right
			right = left + k + 1
		}
	}
	if ans >= math.MaxInt/8 {
		return -1
	}
	return ans
}

func min(a, b int) int { if a < b { return a }; return b }
func max(a, b int) int { if a > b { return a }; return b }

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var n, k int
		fmt.Fscan(in, &n, &k)
		bulbs := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &bulbs[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(bulbs, k))
	}
}
