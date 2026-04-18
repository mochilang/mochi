package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(a []int) int {
	best := 0
	for i := 0; i < len(a); i++ {
		mn := a[i]
		for j := i; j < len(a); j++ {
			if a[j] < mn {
				mn = a[j]
			}
			area := mn * (j - i + 1)
			if area > best {
				best = area
			}
		}
	}
	return best
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
		a := make([]int, n)
		for i := 0; i < n; i++ {
			a[i] = nextInt()
		}
		if tc > 0 {
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(a))
	}
}
