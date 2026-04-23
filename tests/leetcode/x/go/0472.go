package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

func canForm(word string, seen map[string]bool) bool {
	if len(seen) == 0 {
		return false
	}
	dp := make([]bool, len(word)+1)
	dp[0] = true
	for i := 1; i <= len(word); i++ {
		for j := 0; j < i; j++ {
			if dp[j] && seen[word[j:i]] {
				dp[i] = true
				break
			}
		}
	}
	return dp[len(word)]
}

func format(words []string) string {
	sort.Strings(words)
	var b strings.Builder
	b.WriteByte('[')
	for i, w := range words {
		if i > 0 {
			b.WriteByte(',')
		}
		b.WriteByte('"')
		b.WriteString(w)
		b.WriteByte('"')
	}
	b.WriteByte(']')
	return b.String()
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()

	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		words := make([]string, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &words[i])
		}
		ordered := append([]string(nil), words...)
		sort.Slice(ordered, func(i, j int) bool {
			if len(ordered[i]) != len(ordered[j]) {
				return len(ordered[i]) < len(ordered[j])
			}
			return ordered[i] < ordered[j]
		})
		seen := map[string]bool{}
		ans := []string{}
		for _, word := range ordered {
			if canForm(word, seen) {
				ans = append(ans, word)
			}
			seen[word] = true
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, format(ans))
	}
}
