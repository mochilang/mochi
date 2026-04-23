package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type pair struct{ a, b int }

func rev(s string) string { b := []byte(s); for i, j := 0, len(b)-1; i < j; i, j = i+1, j-1 { b[i], b[j] = b[j], b[i] }; return string(b) }
func pal(s string) bool { for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 { if s[i] != s[j] { return false } }; return true }
func palindromePairs(words []string) []pair {
	pos := map[string]int{}; for i, w := range words { pos[w] = i }
	ans := []pair{}
	for i, w := range words {
		for cut := 0; cut <= len(w); cut++ {
			pre, suf := w[:cut], w[cut:]
			if pal(pre) { if j, ok := pos[rev(suf)]; ok && j != i { ans = append(ans, pair{j, i}) } }
			if cut < len(w) && pal(suf) { if j, ok := pos[rev(pre)]; ok && j != i { ans = append(ans, pair{i, j}) } }
		}
	}
	sort.Slice(ans, func(i, j int) bool { if ans[i].a == ans[j].a { return ans[i].b < ans[j].b }; return ans[i].a < ans[j].a })
	return ans
}
func fmtPairs(p []pair) string { parts := make([]string, len(p)); for i, q := range p { parts[i] = fmt.Sprintf("[%d,%d]", q.a, q.b) }; return "[" + strings.Join(parts, ",") + "]" }
func main() { in := bufio.NewReader(os.Stdin); out := bufio.NewWriter(os.Stdout); defer out.Flush(); var t int; if _, err := fmt.Fscan(in, &t); err != nil { return }; for tc := 0; tc < t; tc++ { var n int; fmt.Fscan(in, &n); words := make([]string, n); for i := range words { fmt.Fscan(in, &words[i]); if words[i] == "_" { words[i] = "" } }; if tc > 0 { fmt.Fprintln(out); fmt.Fprintln(out) }; fmt.Fprint(out, fmtPairs(palindromePairs(words))) } }
