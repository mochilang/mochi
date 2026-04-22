package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func wordBreak(s string, words []string) []string {
	wordSet := map[string]bool{}
	lengthSet := map[int]bool{}
	for _, w := range words {
		wordSet[w] = true
		lengthSet[len(w)] = true
	}
	var lengths []int
	for l := range lengthSet {
		lengths = append(lengths, l)
	}
	sort.Ints(lengths)
	memo := map[int][]string{}
	var dfs func(int) []string
	dfs = func(i int) []string {
		if v, ok := memo[i]; ok {
			return v
		}
		if i == len(s) {
			return []string{""}
		}
		var out []string
		for _, length := range lengths {
			j := i + length
			if j > len(s) {
				break
			}
			word := s[i:j]
			if wordSet[word] {
				for _, tail := range dfs(j) {
					if tail == "" {
						out = append(out, word)
					} else {
						out = append(out, word+" "+tail)
					}
				}
			}
		}
		sort.Strings(out)
		memo[i] = out
		return out
	}
	return dfs(0)
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
	tc, _ := strconv.Atoi(lines[0])
	idx := 1
	for t := 0; t < tc; t++ {
		s := lines[idx]
		idx++
		n, _ := strconv.Atoi(lines[idx])
		idx++
		words := append([]string{}, lines[idx:idx+n]...)
		idx += n
		ans := wordBreak(s, words)
		if t > 0 {
			fmt.Print("\n\n")
		}
		fmt.Print(len(ans))
		for _, sentence := range ans {
			fmt.Print("\n")
			fmt.Print(sentence)
		}
	}
}
