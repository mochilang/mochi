package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(costs [][]int) int {
	if len(costs) == 0 {
		return 0
	}
	prev := append([]int(nil), costs[0]...)
	for _, row := range costs[1:] {
		min1, min2, idx1 := 1<<60, 1<<60, -1
		for i, v := range prev {
			if v < min1 {
				min2 = min1
				min1 = v
				idx1 = i
			} else if v < min2 {
				min2 = v
			}
		}
		cur := make([]int, len(row))
		for i, c := range row {
			if i == idx1 {
				cur[i] = c + min2
			} else {
				cur[i] = c + min1
			}
		}
		prev = cur
	}
	ans := prev[0]
	for _, v := range prev[1:] {
		if v < ans {
			ans = v
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
		k := nextInt()
		costs := make([][]int, n)
		for i := 0; i < n; i++ {
			costs[i] = make([]int, k)
			for j := 0; j < k; j++ {
				costs[i][j] = nextInt()
			}
		}
		if tc > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, solve(costs))
	}
}
