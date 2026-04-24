package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Item struct {
	word string
	idx  int
}

func lcp(a string, b string) int {
	i := 0
	for i < len(a) && i < len(b) && a[i] == b[i] {
		i++
	}
	return i
}

func abbreviate(word string, prefix int) string {
	if len(word)-prefix <= 2 {
		return word
	}
	return word[:prefix] + fmt.Sprint(len(word)-prefix-1) + word[len(word)-1:]
}

func solve(words []string) []string {
	groups := map[string][]Item{}
	for i, word := range words {
		key := fmt.Sprintf("%d|%c|%c", len(word), word[0], word[len(word)-1])
		groups[key] = append(groups[key], Item{word: word, idx: i})
	}
	ans := make([]string, len(words))
	for _, group := range groups {
		sort.Slice(group, func(i, j int) bool {
			return group[i].word < group[j].word
		})
		for j, item := range group {
			need := 1
			if j > 0 {
				x := lcp(item.word, group[j-1].word) + 1
				if x > need {
					need = x
				}
			}
			if j+1 < len(group) {
				x := lcp(item.word, group[j+1].word) + 1
				if x > need {
					need = x
				}
			}
			ans[item.idx] = abbreviate(item.word, need)
		}
	}
	return ans
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	blocks := make([]string, 0)
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		words := make([]string, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &words[i])
		}
		ans := solve(words)
		if tc > 0 {
			blocks = append(blocks, "")
		}
		blocks = append(blocks, fmt.Sprint(len(ans)))
		blocks = append(blocks, ans...)
	}
	fmt.Print(strings.Join(blocks, "\n"))
}
