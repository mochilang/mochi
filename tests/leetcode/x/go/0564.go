package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func makePal(prefix string, odd bool) int64 {
	s := prefix
	if odd {
		for i := len(prefix) - 2; i >= 0; i-- {
			s += string(prefix[i])
		}
	} else {
		for i := len(prefix) - 1; i >= 0; i-- {
			s += string(prefix[i])
		}
	}
	v, _ := strconv.ParseInt(s, 10, 64)
	return v
}

func solve(n string) string {
	m := len(n)
	x, _ := strconv.ParseInt(n, 10, 64)
	cands := map[int64]bool{
		int64Pow(10, m-1) - 1: true,
		int64Pow(10, m) + 1:   true,
	}
	prefix, _ := strconv.ParseInt(n[:(m+1)/2], 10, 64)
	for _, d := range []int64{-1, 0, 1} {
		p := strconv.FormatInt(prefix+d, 10)
		cands[makePal(p, m%2 == 1)] = true
	}
	delete(cands, x)
	var best int64
	first := true
	for cand := range cands {
		if cand < 0 {
			continue
		}
		if first || abs64(cand-x) < abs64(best-x) || (abs64(cand-x) == abs64(best-x) && cand < best) {
			best = cand
			first = false
		}
	}
	return strconv.FormatInt(best, 10)
}

func int64Pow(a int64, b int) int64 {
	res := int64(1)
	for ; b > 0; b-- {
		res *= a
	}
	return res
}

func abs64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for i := 0; i < t; i++ {
		var n string
		fmt.Fscan(in, &n)
		out = append(out, solve(n))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
