package main

import (
	"bufio"
	"fmt"
	"os"
)

func feasible(nums []int, k int, mid float64) bool {
	total, prev, minPrev := 0.0, 0.0, 0.0
	for i, x := range nums {
		total += float64(x) - mid
		if i >= k {
			prev += float64(nums[i-k]) - mid
			if prev < minPrev {
				minPrev = prev
			}
		}
		if i+1 >= k && total-minPrev >= 0 {
			return true
		}
	}
	return false
}

func solve(nums []int, k int) float64 {
	lo, hi := float64(nums[0]), float64(nums[0])
	for _, x := range nums {
		v := float64(x)
		if v < lo {
			lo = v
		}
		if v > hi {
			hi = v
		}
	}
	for i := 0; i < 60; i++ {
		mid := (lo + hi) / 2
		if feasible(nums, k, mid) {
			lo = mid
		} else {
			hi = mid
		}
	}
	return lo
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
		var n, k int
		fmt.Fscan(in, &n, &k)
		nums := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &nums[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprintf(out, "%.5f", solve(nums, k))
	}
}
