package main

import (
    "fmt"
    "sort"
    "strings"
)

func ladders(begin, end string, words []string) [][]string {
    wordSet := map[string]bool{}
    for _, w := range words { wordSet[w] = true }
    if !wordSet[end] { return [][]string{} }
    parents := map[string][]string{}
    level := map[string]bool{begin: true}
    visited := map[string]bool{begin: true}
    found := false
    for len(level) > 0 && !found {
        nextLevel := map[string]bool{}
        curWords := make([]string, 0, len(level))
        for w := range level { curWords = append(curWords, w) }
        sort.Strings(curWords)
        for _, word := range curWords {
            chars := []byte(word)
            for i, orig := range chars {
                for c := byte('a'); c <= 'z'; c++ {
                    if c == orig { continue }
                    chars[i] = c
                    nw := string(chars)
                    if !wordSet[nw] || visited[nw] { continue }
                    nextLevel[nw] = true
                    parents[nw] = append(parents[nw], word)
                    if nw == end { found = true }
                }
                chars[i] = orig
            }
        }
        for w := range nextLevel { visited[w] = true }
        level = nextLevel
    }
    if !found { return [][]string{} }
    var out [][]string
    path := []string{end}
    var backtrack func(string)
    backtrack = func(word string) {
        if word == begin {
            seq := make([]string, len(path))
            for i := range path { seq[i] = path[len(path)-1-i] }
            out = append(out, seq)
            return
        }
        plist := append([]string{}, parents[word]...)
        sort.Strings(plist)
        for _, p := range plist {
            path = append(path, p)
            backtrack(p)
            path = path[:len(path)-1]
        }
    }
    backtrack(end)
    sort.Slice(out, func(i, j int) bool { return strings.Join(out[i], "->") < strings.Join(out[j], "->") })
    return out
}

func fmtPaths(paths [][]string) string {
    lines := []string{fmt.Sprint(len(paths))}
    for _, p := range paths { lines = append(lines, strings.Join(p, "->")) }
    return strings.Join(lines, "\n")
}

func main() {
    var tc int
    if _, err := fmt.Scan(&tc); err != nil { return }
    out := make([]string, 0, tc)
    for t := 0; t < tc; t++ {
        var begin, end string
        var n int
        fmt.Scan(&begin, &end, &n)
        words := make([]string, n)
        for i := 0; i < n; i++ { fmt.Scan(&words[i]) }
        out = append(out, fmtPaths(ladders(begin, end, words)))
    }
    fmt.Print(strings.Join(out, "\n\n"))
}
