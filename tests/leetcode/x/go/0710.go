package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Solution struct {
	bound int
	remap map[int]int
	state uint32
}

func NewSolution(n int, blacklist []int) *Solution {
	blocked := map[int]bool{}
	for _, value := range blacklist {
		blocked[value] = true
	}
	bound := n - len(blacklist)
	tail := []int{}
	for value := bound; value < n; value++ {
		if !blocked[value] {
			tail = append(tail, value)
		}
	}
	remap := map[int]int{}
	idx := 0
	for _, value := range blacklist {
		if value < bound {
			remap[value] = tail[idx]
			idx++
		}
	}
	return &Solution{bound: bound, remap: remap, state: 1}
}

func (s *Solution) Pick() int {
	s.state = s.state*1664525 + 1013904223
	value := int(s.state % uint32(s.bound))
	if mapped, ok := s.remap[value]; ok {
		return mapped
	}
	return value
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	cases := []string{}
	for ; t > 0; t-- {
		var ops int
		fmt.Fscan(in, &ops)
		res := []string{}
		var solution *Solution
		for i := 0; i < ops; i++ {
			var op string
			fmt.Fscan(in, &op)
			if op == "C" {
				var n, b int
				fmt.Fscan(in, &n, &b)
				blacklist := make([]int, b)
				for j := 0; j < b; j++ {
					fmt.Fscan(in, &blacklist[j])
				}
				solution = NewSolution(n, blacklist)
				res = append(res, "null")
			} else {
				res = append(res, fmt.Sprint(solution.Pick()))
			}
		}
		cases = append(cases, "["+strings.Join(res, ",")+"]")
	}
	fmt.Fprint(out, strings.Join(cases, "\n\n"))
}
