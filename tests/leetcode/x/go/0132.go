package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func minCut(s string) int {
	n := len(s)
	pal := make([][]bool, n)
	for i := range pal {
		pal[i] = make([]bool, n)
	}
	cuts := make([]int, n)
	for end := 0; end < n; end++ {
		cuts[end] = end
		for start := 0; start <= end; start++ {
			if s[start] == s[end] && (end-start <= 2 || pal[start+1][end-1]) {
				pal[start][end] = true
				if start == 0 {
					cuts[end] = 0
				} else if cuts[start-1]+1 < cuts[end] {
					cuts[end] = cuts[start-1] + 1
				}
			}
		}
	}
	return cuts[n-1]
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	var lines []string
	for in.Scan() {
		lines = append(lines, in.Text())
	}
	if len(lines) == 0 {
		return
	}
	tc, _ := strconv.Atoi(lines[0])
	for i := 1; i <= tc; i++ {
		if i > 1 {
			fmt.Print("\n\n")
		}
		fmt.Print(minCut(lines[i]))
	}
}
