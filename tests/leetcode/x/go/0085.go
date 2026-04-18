package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func hist(h []int) int {
	best := 0
	for i := 0; i < len(h); i++ {
		mn := h[i]
		for j := i; j < len(h); j++ {
			if h[j] < mn {
				mn = h[j]
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
	next := func() string { in.Scan(); return in.Text() }
	if !in.Scan() { return }
	t, _ := strconv.Atoi(in.Text())
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		rows, _ := strconv.Atoi(next())
		cols, _ := strconv.Atoi(next())
		h := make([]int, cols)
		best := 0
		for r := 0; r < rows; r++ {
			s := next()
			for c := 0; c < cols; c++ {
				if s[c] == '1' { h[c]++ } else { h[c] = 0 }
			}
			if area := hist(h); area > best { best = area }
		}
		if tc > 0 { fmt.Fprintln(out) }
		fmt.Fprint(out, best)
	}
}
