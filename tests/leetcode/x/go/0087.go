package main

import (
	"bufio"
	"fmt"
	"os"
)

func isScramble(s1, s2 string) bool {
	n := len(s1)
	memo := map[[3]int]bool{}
	seen := map[[3]int]bool{}
	var dfs func(int, int, int) bool
	dfs = func(i1, i2, l int) bool {
		key := [3]int{i1, i2, l}
		if seen[key] {
			return memo[key]
		}
		seen[key] = true
		a, b := s1[i1:i1+l], s2[i2:i2+l]
		if a == b {
			memo[key] = true
			return true
		}
		var c [26]int
		for i := 0; i < l; i++ {
			c[a[i]-'a']++
			c[b[i]-'a']--
		}
		for _, v := range c {
			if v != 0 {
				memo[key] = false
				return false
			}
		}
		for k := 1; k < l; k++ {
			if dfs(i1, i2, k) && dfs(i1+k, i2+k, l-k) || dfs(i1, i2+l-k, k) && dfs(i1+k, i2, l-k) {
				memo[key] = true
				return true
			}
		}
		memo[key] = false
		return false
	}
	return dfs(0, 0, n)
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	lines := []string{}
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}
	if len(lines) == 0 {
		return
	}
	var t int
	fmt.Sscanf(lines[0], "%d", &t)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for i := 0; i < t; i++ {
		if i > 0 {
			fmt.Fprintln(out)
		}
		if isScramble(lines[1+2*i], lines[2+2*i]) {
			fmt.Fprint(out, "true")
		} else {
			fmt.Fprint(out, "false")
		}
	}
}
