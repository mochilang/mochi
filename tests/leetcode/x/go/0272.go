package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func solve(values []int, target float64, k int) []int {
	i := sort.Search(len(values), func(i int) bool { return float64(values[i]) >= target })
	left, right := i-1, i
	ans := make([]int, 0, k)
	for len(ans) < k {
		if left < 0 {
			ans = append(ans, values[right])
			right++
		} else if right >= len(values) {
			ans = append(ans, values[left])
			left--
		} else if target-float64(values[left]) <= float64(values[right])-target {
			ans = append(ans, values[left])
			left--
		} else {
			ans = append(ans, values[right])
			right++
		}
	}
	return ans
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	next := func() string { sc.Scan(); return sc.Text() }
	nextInt := func() int { v, _ := strconv.Atoi(next()); return v }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		n := nextInt()
		values := make([]int, n)
		for i := 0; i < n; i++ {
			values[i] = nextInt()
		}
		target, _ := strconv.ParseFloat(next(), 64)
		k := nextInt()
		ans := solve(values, target, k)
		if tc > 0 {
			fmt.Fprint(w, "\n\n")
		}
		fmt.Fprintln(w, len(ans))
		for i, v := range ans {
			if i > 0 {
				fmt.Fprintln(w)
			}
			fmt.Fprint(w, v)
		}
	}
}
