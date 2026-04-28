package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"
)

type AutocompleteSystem struct {
	counts  map[string]int
	current string
}

func newSystem(sentences []string, times []int) *AutocompleteSystem {
	c := map[string]int{}
	for i, s := range sentences {
		c[s] += times[i]
	}
	return &AutocompleteSystem{counts: c}
}

func (a *AutocompleteSystem) input(ch string) []string {
	if ch == "#" {
		a.counts[a.current]++
		a.current = ""
		return []string{}
	}
	a.current += ch
	matches := make([]string, 0)
	for s := range a.counts {
		if strings.HasPrefix(s, a.current) {
			matches = append(matches, s)
		}
	}
	sort.Slice(matches, func(i, j int) bool {
		if a.counts[matches[i]] != a.counts[matches[j]] {
			return a.counts[matches[i]] > a.counts[matches[j]]
		}
		return matches[i] < matches[j]
	})
	if len(matches) > 3 {
		matches = matches[:3]
	}
	return matches
}

func formatList(items []string, ok bool) string {
	if !ok {
		return "null"
	}
	parts := make([]string, len(items))
	for i, s := range items {
		parts[i] = `"` + s + `"`
	}
	return "[" + strings.Join(parts, ",") + "]"
}

func decodeChar(s string) string {
	if s == "<space>" {
		return " "
	}
	return s
}

func main() {
	f, _ := io.ReadAll(os.Stdin)
	lines0 := strings.Split(string(f), "\n")
	lines := make([]string, 0, len(lines0))
	for _, line := range lines0 {
		if strings.TrimSpace(line) != "" {
			lines = append(lines, strings.TrimRight(line, "\r"))
		}
	}
	if len(lines) == 0 {
		return
	}
	t, _ := strconv.Atoi(lines[0])
	idx := 1
	cases := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		q, _ := strconv.Atoi(lines[idx])
		idx++
		out := make([]string, 0, q)
		var sys *AutocompleteSystem
		for i := 0; i < q; i++ {
			line := lines[idx]
			idx++
			if strings.HasPrefix(line, "C ") {
				n, _ := strconv.Atoi(strings.Fields(line)[1])
				sentences := make([]string, n)
				times := make([]int, n)
				for j := 0; j < n; j++ {
					parts := strings.SplitN(lines[idx], "|", 2)
					idx++
					times[j], _ = strconv.Atoi(parts[0])
					sentences[j] = parts[1]
				}
				sys = newSystem(sentences, times)
				out = append(out, "null")
			} else {
				ch := decodeChar(line[2:])
				out = append(out, formatList(sys.input(ch), true))
			}
		}
		cases = append(cases, "["+strings.Join(out, ",")+"]")
	}
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	fmt.Fprint(w, strings.Join(cases, "\n\n"))
}
