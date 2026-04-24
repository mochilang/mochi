package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func solve(machines []int) int {
	total := 0
	for _, x := range machines {
		total += x
	}
	n := len(machines)
	if total%n != 0 {
		return -1
	}
	target := total / n
	flow := 0
	ans := 0
	for _, x := range machines {
		diff := x - target
		flow += diff
		if abs(flow) > ans {
			ans = abs(flow)
		}
		if diff > ans {
			ans = diff
		}
	}
	return ans
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		machines := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &machines[i])
		}
		out = append(out, fmt.Sprint(solve(machines)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
