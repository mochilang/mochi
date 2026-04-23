package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

func value(base int64, m int, limit int64) int64 {
	total, cur := int64(1), int64(1)
	for i := 0; i < m; i++ {
		if cur > limit/base {
			return limit + 1
		}
		cur *= base
		if total > limit-cur {
			return limit + 1
		}
		total += cur
	}
	return total
}

func solve(n int64) string {
	maxM := bitsLen(n) - 1
	for m := maxM; m >= 2; m-- {
		lo, hi := int64(2), int64(math.Pow(float64(n), 1.0/float64(m)))+1
		for lo <= hi {
			mid := (lo + hi) / 2
			s := value(mid, m, n)
			if s == n {
				return strconv.FormatInt(mid, 10)
			}
			if s < n {
				lo = mid + 1
			} else {
				hi = mid - 1
			}
		}
	}
	return strconv.FormatInt(n-1, 10)
}

func bitsLen(x int64) int {
	n := 0
	for x > 0 {
		n++
		x >>= 1
	}
	return n
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
		var n int64
		fmt.Fscan(in, &n)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(n))
	}
}
