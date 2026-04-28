package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func solve(n int) int {
	f := make([]int, 32)
	f[0] = 1
	f[1] = 2
	for i := 2; i < 32; i++ {
		f[i] = f[i-1] + f[i-2]
	}

	ans := 0
	prev := 0
	for i := 30; i >= 0; i-- {
		if n&(1<<i) != 0 {
			ans += f[i]
			if prev == 1 {
				return ans
			}
			prev = 1
		} else {
			prev = 0
		}
	}
	return ans + 1
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for i := 0; i < t; i++ {
		var n int
		fmt.Fscan(in, &n)
		out = append(out, fmt.Sprint(solve(n)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
