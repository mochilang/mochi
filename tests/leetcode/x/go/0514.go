package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(ring string, key string) int {
	n := len(ring)
	positions := map[byte][]int{}
	for i := 0; i < n; i++ {
		ch := ring[i]
		positions[ch] = append(positions[ch], i)
	}
	dp := map[int]int{0: 0}
	for i := 0; i < len(key); i++ {
		ch := key[i]
		next := map[int]int{}
		for _, j := range positions[ch] {
			best := int(1e9)
			for p, cost := range dp {
				diff := p - j
				if diff < 0 {
					diff = -diff
				}
				step := diff
				if n-diff < step {
					step = n - diff
				}
				cand := cost + step
				if cand < best {
					best = cand
				}
			}
			next[j] = best
		}
		dp = next
	}
	ans := int(1e9)
	for _, cost := range dp {
		if cost < ans {
			ans = cost
		}
	}
	return ans + len(key)
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		var ring, key string
		fmt.Fscan(in, &ring)
		fmt.Fscan(in, &key)
		out = append(out, fmt.Sprint(solve(ring, key)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
