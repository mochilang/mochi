package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

func countRangeSum(nums []int, lower int, upper int) int {
	prefix := make([]int64, len(nums)+1)
	for i, x := range nums {
		prefix[i+1] = prefix[i] + int64(x)
	}
	var sortCount func(int, int) int
	sortCount = func(lo, hi int) int {
		if hi-lo <= 1 {
			return 0
		}
		mid := (lo + hi) / 2
		ans := sortCount(lo, mid) + sortCount(mid, hi)
		left, right := lo, lo
		for r := mid; r < hi; r++ {
			for left < mid && prefix[left] < prefix[r]-int64(upper) {
				left++
			}
			for right < mid && prefix[right] <= prefix[r]-int64(lower) {
				right++
			}
			ans += right - left
		}
		sort.Slice(prefix[lo:hi], func(i, j int) bool { return prefix[lo+i] < prefix[lo+j] })
		return ans
	}
	return sortCount(0, len(prefix))
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
		var n int
		fmt.Fscan(in, &n)
		nums := make([]int, n)
		for i := range nums {
			fmt.Fscan(in, &nums[i])
		}
		var lower, upper int
		fmt.Fscan(in, &lower, &upper)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, countRangeSum(nums, lower, upper))
	}
}
