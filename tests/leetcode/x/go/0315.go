package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func countSmaller(nums []int) []int {
	n := len(nums)
	counts := make([]int, n)
	idx := make([]int, n)
	tmp := make([]int, n)
	for i := range idx {
		idx[i] = i
	}
	var sort func(int, int)
	sort = func(lo, hi int) {
		if hi-lo <= 1 {
			return
		}
		mid := (lo + hi) / 2
		sort(lo, mid)
		sort(mid, hi)
		i, j, k, moved := lo, mid, lo, 0
		for i < mid && j < hi {
			if nums[idx[j]] < nums[idx[i]] {
				tmp[k] = idx[j]
				j++
				moved++
			} else {
				counts[idx[i]] += moved
				tmp[k] = idx[i]
				i++
			}
			k++
		}
		for i < mid {
			counts[idx[i]] += moved
			tmp[k] = idx[i]
			i++
			k++
		}
		for j < hi {
			tmp[k] = idx[j]
			j++
			k++
		}
		copy(idx[lo:hi], tmp[lo:hi])
	}
	sort(0, n)
	return counts
}

func fmtList(a []int) string {
	parts := make([]string, len(a))
	for i, v := range a {
		parts[i] = strconv.Itoa(v)
	}
	return "[" + strings.Join(parts, ",") + "]"
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
		fmt.Fprint(out, fmtList(countSmaller(nums)))
	}
}
