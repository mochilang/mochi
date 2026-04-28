package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(positions [][2]int) []int {
	heights := make([]int, len(positions))
	answer := make([]int, 0, len(positions))
	best := 0
	for i, current := range positions {
		left, size := current[0], current[1]
		right := left + size
		base := 0
		for j := 0; j < i; j++ {
			otherLeft, otherSize := positions[j][0], positions[j][1]
			otherRight := otherLeft + otherSize
			if left < otherRight && otherLeft < right && heights[j] > base {
				base = heights[j]
			}
		}
		heights[i] = base + size
		if heights[i] > best {
			best = heights[i]
		}
		answer = append(answer, best)
	}
	return answer
}

func fmtArr(values []int) string {
	parts := make([]string, len(values))
	for i, value := range values {
		parts[i] = fmt.Sprint(value)
	}
	return "[" + strings.Join(parts, ",") + "]"
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
		positions := make([][2]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &positions[i][0], &positions[i][1])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, fmtArr(solve(positions)))
	}
}
