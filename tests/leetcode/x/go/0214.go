package main

import (
	"bufio"
	"fmt"
	"os"
)

func solve(s string) string {
	revBytes := []byte(s)
	for i, j := 0, len(revBytes)-1; i < j; i, j = i+1, j-1 {
		revBytes[i], revBytes[j] = revBytes[j], revBytes[i]
	}
	rev := string(revBytes)
	combined := s + "#" + rev
	pi := make([]int, len(combined))
	for i := 1; i < len(combined); i++ {
		j := pi[i-1]
		for j > 0 && combined[i] != combined[j] {
			j = pi[j-1]
		}
		if combined[i] == combined[j] {
			j++
		}
		pi[i] = j
	}
	keep := 0
	if len(pi) > 0 {
		keep = pi[len(pi)-1]
	}
	return rev[:len(s)-keep] + s
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
	fmt.Sscanf(lines[0], "%d", &t)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for i := 0; i < t; i++ {
		if i > 0 {
			fmt.Fprintln(out)
		}
		s := ""
		if i+1 < len(lines) {
			s = lines[i+1]
		}
		fmt.Fprint(out, solve(s))
	}
}
