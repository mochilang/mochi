package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func ladderLength(begin, end string, words []string) int {
	wordSet := map[string]bool{}
	for _, word := range words {
		wordSet[word] = true
	}
	if !wordSet[end] {
		return 0
	}
	level := map[string]bool{begin: true}
	visited := map[string]bool{begin: true}
	steps := 1
	for len(level) > 0 {
		if level[end] {
			return steps
		}
		next := map[string]bool{}
		cur := make([]string, 0, len(level))
		for word := range level {
			cur = append(cur, word)
		}
		sort.Strings(cur)
		for _, word := range cur {
			chars := []byte(word)
			for i, orig := range chars {
				for c := byte('a'); c <= 'z'; c++ {
					if c == orig {
						continue
					}
					chars[i] = c
					nw := string(chars)
					if wordSet[nw] && !visited[nw] {
						next[nw] = true
					}
				}
				chars[i] = orig
			}
		}
		for word := range next {
			visited[word] = true
		}
		level = next
		steps++
	}
	return 0
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
	var out []string
	for t := 0; t < tc; t++ {
		begin := lines[idx]
		idx++
		end := lines[idx]
		idx++
		n, _ := strconv.Atoi(lines[idx])
		idx++
		words := append([]string{}, lines[idx:idx+n]...)
		idx += n
		out = append(out, strconv.Itoa(ladderLength(begin, end, words)))
	}
	for i, s := range out {
		if i > 0 {
			fmt.Print("\n\n")
		}
		fmt.Print(s)
	}
}
