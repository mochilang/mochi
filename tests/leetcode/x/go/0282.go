package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func solve(num string, target int64) []string {
	ans := []string{}
	var dfs func(int, string, int64, int64)
	dfs = func(i int, expr string, value int64, last int64) {
		if i == len(num) {
			if value == target {
				ans = append(ans, expr)
			}
			return
		}
		for j := i; j < len(num); j++ {
			if j > i && num[i] == '0' {
				break
			}
			s := num[i : j+1]
			n, _ := strconv.ParseInt(s, 10, 64)
			if i == 0 {
				dfs(j+1, s, n, n)
			} else {
				dfs(j+1, expr+"+"+s, value+n, n)
				dfs(j+1, expr+"-"+s, value-n, -n)
				dfs(j+1, expr+"*"+s, value-last+last*n, last*n)
			}
		}
	}
	dfs(0, "", 0, 0)
	sort.Strings(ans)
	return ans
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	var lines []string
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}
	if len(lines) == 0 {
		return
	}
	t, _ := strconv.Atoi(strings.TrimSpace(lines[0]))
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	idx := 1
	for tc := 0; tc < t; tc++ {
		num := strings.TrimSpace(lines[idx])
		target, _ := strconv.ParseInt(strings.TrimSpace(lines[idx+1]), 10, 64)
		idx += 2
		ans := solve(num, target)
		if tc > 0 {
			fmt.Fprint(out, "\n\n")
		}
		fmt.Fprint(out, len(ans))
		for _, s := range ans {
			fmt.Fprint(out, "\n", s)
		}
	}
}
