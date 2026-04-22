package main

import (
	"bufio"
	"fmt"
	"os"
)

var pairs = [][2]string{{"0", "0"}, {"1", "1"}, {"6", "9"}, {"8", "8"}, {"9", "6"}}

func build(n, m int) []string {
	if n == 0 {
		return []string{""}
	}
	if n == 1 {
		return []string{"0", "1", "8"}
	}
	mids := build(n-2, m)
	res := make([]string, 0, len(mids)*5)
	for _, mid := range mids {
		for _, p := range pairs {
			if n == m && p[0] == "0" {
				continue
			}
			res = append(res, p[0]+mid+p[1])
		}
	}
	return res
}

func countRange(low, high string) int {
	ans := 0
	for length := len(low); length <= len(high); length++ {
		for _, s := range build(length, length) {
			if length == len(low) && s < low {
				continue
			}
			if length == len(high) && s > high {
				continue
			}
			ans++
		}
	}
	return ans
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
	var t int
	fmt.Sscanf(lines[0], "%d", &t)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	idx := 1
	for i := 0; i < t; i++ {
		if i > 0 {
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, countRange(lines[idx], lines[idx+1]))
		idx += 2
	}
}
