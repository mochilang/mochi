//go:build ignore

package main

import (
    "fmt"
    "sort"
)

type Item struct {
    Cat  string
    Val  int
    Flag bool
}

type Result struct {
    Cat   string
    Share float64
}

func main() {
    items := []Item{
        {Cat: "a", Val: 10, Flag: true},
        {Cat: "a", Val: 5, Flag: false},
        {Cat: "b", Val: 20, Flag: true},
    }

    groups := make(map[string][]Item)
    for _, it := range items {
        groups[it.Cat] = append(groups[it.Cat], it)
    }

    var results []Result
    for cat, group := range groups {
        var total, selected int
        for _, it := range group {
            total += it.Val
            if it.Flag {
                selected += it.Val
            }
        }
        share := float64(selected) / float64(total)
        results = append(results, Result{cat, share})
    }

    sort.Slice(results, func(i, j int) bool { return results[i].Cat < results[j].Cat })

    fmt.Println(results)
}
