package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(nums []int, k int) []int {
	n := len(nums)
	m := n - k + 1
	windows := make([]int, m)
	sum := 0
	for i := 0; i < n; i++ {
		sum += nums[i]
		if i >= k {
			sum -= nums[i-k]
		}
		if i >= k-1 {
			windows[i-k+1] = sum
		}
	}
	left := make([]int, m)
	best := 0
	for i := 0; i < m; i++ {
		if windows[i] > windows[best] {
			best = i
		}
		left[i] = best
	}
	right := make([]int, m)
	best = m - 1
	for i := m - 1; i >= 0; i-- {
		if windows[i] >= windows[best] {
			best = i
		}
		right[i] = best
	}
	ans := []int{0, 0, 0}
	bestSum := -1
	for mid := k; mid < m-k; mid++ {
		a, c := left[mid-k], right[mid+k]
		total := windows[a] + windows[mid] + windows[c]
		cand := []int{a, mid, c}
		if total > bestSum || total == bestSum && less(cand, ans) {
			bestSum = total
			ans = cand
		}
	}
	return ans
}

func less(a, b []int) bool {
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return a[i] < b[i]
		}
	}
	return false
}

func fmtArr(arr []int) string {
	parts := make([]string, len(arr))
	for i, x := range arr {
		parts[i] = fmt.Sprint(x)
	}
	return "[" + strings.Join(parts, ",") + "]"
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
		fmt.Fprint(out, fmtArr(solve(nums, k)))
	}
}
