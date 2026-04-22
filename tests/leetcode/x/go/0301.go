package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

func solve(s string) []string {
	leftRemove, rightRemove := 0, 0
	for _, ch := range s {
		if ch == '(' {
			leftRemove++
		} else if ch == ')' {
			if leftRemove > 0 {
				leftRemove--
			} else {
				rightRemove++
			}
		}
	}

	ans := map[string]bool{}
	var path []rune
	var dfs func(int, int, int, int)
	chars := []rune(s)
	dfs = func(i, left, right, balance int) {
		if i == len(chars) {
			if left == 0 && right == 0 && balance == 0 {
				ans[string(path)] = true
			}
			return
		}
		ch := chars[i]
		if ch == '(' {
			if left > 0 {
				dfs(i+1, left-1, right, balance)
			}
			path = append(path, ch)
			dfs(i+1, left, right, balance+1)
			path = path[:len(path)-1]
		} else if ch == ')' {
			if right > 0 {
				dfs(i+1, left, right-1, balance)
			}
			if balance > 0 {
				path = append(path, ch)
				dfs(i+1, left, right, balance-1)
				path = path[:len(path)-1]
			}
		} else {
			path = append(path, ch)
			dfs(i+1, left, right, balance)
			path = path[:len(path)-1]
		}
	}
	dfs(0, leftRemove, rightRemove, 0)

	out := make([]string, 0, len(ans))
	for s := range ans {
		out = append(out, s)
	}
	sort.Strings(out)
	return out
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	lines := []string{}
	for in.Scan() {
		lines = append(lines, in.Text())
	}
	if len(lines) == 0 {
		return
	}
	var t int
	fmt.Sscan(lines[0], &t)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		ans := solve(lines[tc+1])
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprintln(out, len(ans))
		for i, s := range ans {
			if i > 0 {
				fmt.Fprintln(out)
			}
			fmt.Fprint(out, s)
		}
	}
}
