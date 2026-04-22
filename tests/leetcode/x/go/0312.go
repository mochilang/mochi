package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func maxCoins(nums []int) int {
	vals := make([]int, 0, len(nums)+2)
	vals = append(vals, 1)
	vals = append(vals, nums...)
	vals = append(vals, 1)
	n := len(vals)
	dp := make([][]int, n)
	for i := range dp {
		dp[i] = make([]int, n)
	}
	for length := 2; length < n; length++ {
		for left := 0; left+length < n; left++ {
			right := left + length
			best := 0
			for k := left + 1; k < right; k++ {
				cand := dp[left][k] + dp[k][right] + vals[left]*vals[k]*vals[right]
				if cand > best {
					best = cand
				}
			}
			dp[left][right] = best
		}
	}
	return dp[0][n-1]
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
		n := nextInt()
		nums := make([]int, n)
		for i := 0; i < n; i++ {
			nums[i] = nextInt()
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, maxCoins(nums))
	}
}
