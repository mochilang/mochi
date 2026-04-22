package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(nums []int, k int) []int {
	dq := make([]int, 0, len(nums))
	ans := make([]int, 0, len(nums)-k+1)
	for i, x := range nums {
		if len(dq) > 0 && dq[0] <= i-k {
			dq = dq[1:]
		}
		for len(dq) > 0 && nums[dq[len(dq)-1]] <= x {
			dq = dq[:len(dq)-1]
		}
		dq = append(dq, i)
		if i >= k-1 {
			ans = append(ans, nums[dq[0]])
		}
	}
	return ans
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
		n := nextInt()
		nums := make([]int, n)
		for i := range nums {
			nums[i] = nextInt()
		}
		k := nextInt()
		ans := solve(nums, k)
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
