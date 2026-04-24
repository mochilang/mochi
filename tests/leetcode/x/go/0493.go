package main

import (
	"bufio"
	"fmt"
	"os"
)

func sortCount(a []int64, l, r int, tmp []int64) int64 {
	if r-l <= 1 {
		return 0
	}
	m := (l + r) / 2
	cnt := sortCount(a, l, m, tmp) + sortCount(a, m, r, tmp)
	j := m
	for i := l; i < m; i++ {
		for j < r && a[i] > 2*a[j] {
			j++
		}
		cnt += int64(j - m)
	}
	i, k := l, l
	j = m
	for i < m && j < r {
		if a[i] <= a[j] {
			tmp[k] = a[i]
			i++
		} else {
			tmp[k] = a[j]
			j++
		}
		k++
	}
	for i < m {
		tmp[k] = a[i]
		i++
		k++
	}
	for j < r {
		tmp[k] = a[j]
		j++
		k++
	}
	for x := l; x < r; x++ {
		a[x] = tmp[x]
	}
	return cnt
}

func solve(nums []int64) int64 {
	tmp := make([]int64, len(nums))
	return sortCount(nums, 0, len(nums), tmp)
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
		nums := make([]int64, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &nums[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(nums))
	}
}
