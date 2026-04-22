package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func solve(words []string) string {
	chars := map[byte]bool{}
	for _, w := range words {
		for i := 0; i < len(w); i++ {
			chars[w[i]] = true
		}
	}
	adj := map[byte]map[byte]bool{}
	indeg := map[byte]int{}
	for c := range chars {
		adj[c] = map[byte]bool{}
		indeg[c] = 0
	}
	for i := 0; i+1 < len(words); i++ {
		a, b := words[i], words[i+1]
		m := len(a)
		if len(b) < m {
			m = len(b)
		}
		if a[:m] == b[:m] && len(a) > len(b) {
			return ""
		}
		for j := 0; j < m; j++ {
			if a[j] != b[j] {
				if !adj[a[j]][b[j]] {
					adj[a[j]][b[j]] = true
					indeg[b[j]]++
				}
				break
			}
		}
	}
	zero := make([]byte, 0)
	for c := range chars {
		if indeg[c] == 0 {
			zero = append(zero, c)
		}
	}
	sort.Slice(zero, func(i, j int) bool { return zero[i] < zero[j] })
	out := make([]byte, 0, len(chars))
	for len(zero) > 0 {
		c := zero[0]
		zero = zero[1:]
		out = append(out, c)
		nexts := make([]byte, 0, len(adj[c]))
		for nei := range adj[c] {
			nexts = append(nexts, nei)
		}
		sort.Slice(nexts, func(i, j int) bool { return nexts[i] < nexts[j] })
		for _, nei := range nexts {
			indeg[nei]--
			if indeg[nei] == 0 {
				zero = append(zero, nei)
			}
		}
		sort.Slice(zero, func(i, j int) bool { return zero[i] < zero[j] })
	}
	if len(out) != len(chars) {
		return ""
	}
	return string(out)
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
	t, _ := strconv.Atoi(lines[0])
	idx := 1
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		n, _ := strconv.Atoi(lines[idx])
		idx++
		words := append([]string(nil), lines[idx:idx+n]...)
		idx += n
		if tc > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, solve(words))
	}
}
