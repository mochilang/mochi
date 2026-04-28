package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

const eps = 1e-6

func solve(cards []int) bool {
	nums := make([]float64, len(cards))
	for i, x := range cards {
		nums[i] = float64(x)
	}
	var dfs func([]float64) bool
	dfs = func(arr []float64) bool {
		if len(arr) == 1 {
			return math.Abs(arr[0]-24.0) < eps
		}
		n := len(arr)
		for i := 0; i < n; i++ {
			for j := i + 1; j < n; j++ {
				rest := make([]float64, 0, n-1)
				for k := 0; k < n; k++ {
					if k != i && k != j {
						rest = append(rest, arr[k])
					}
				}
				a, b := arr[i], arr[j]
				cands := []float64{a + b, a * b, a - b, b - a}
				if math.Abs(b) > eps {
					cands = append(cands, a/b)
				}
				if math.Abs(a) > eps {
					cands = append(cands, b/a)
				}
				for _, v := range cands {
					next := append(rest, v)
					if dfs(next) {
						return true
					}
				}
			}
		}
		return false
	}
	return dfs(nums)
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for i := 0; i < t; i++ {
		cards := make([]int, 4)
		for j := 0; j < 4; j++ {
			fmt.Fscan(in, &cards[j])
		}
		if i > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		if solve(cards) {
			fmt.Fprint(out, "true")
		} else {
			fmt.Fprint(out, "false")
		}
	}
}
