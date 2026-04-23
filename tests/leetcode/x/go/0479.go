package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func makePal(left int64) int64 {
	s := strconv.FormatInt(left, 10)
	p := s
	for i := len(s) - 1; i >= 0; i-- {
		p += string(s[i])
	}
	v, _ := strconv.ParseInt(p, 10, 64)
	return v
}

func solve(n int) int64 {
	if n == 1 {
		return 9
	}
	upper := int64(1)
	for i := 0; i < n; i++ {
		upper *= 10
	}
	lower := upper / 10
	upper--
	for left := upper; left >= lower; left-- {
		pal := makePal(left)
		for x := upper; x*x >= pal; x-- {
			if pal%x == 0 {
				y := pal / x
				if y >= lower && y <= upper {
					return pal % 1337
				}
			}
		}
	}
	return -1
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
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(n))
	}
}
