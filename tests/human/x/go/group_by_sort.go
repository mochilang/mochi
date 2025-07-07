//go:build ignore

package main

import (
	"fmt"
	"sort"
)

type Item struct {
	Cat string
	Val int
}

type Result struct {
	Cat   string
	Total int
}

func main() {
	items := []Item{{"a", 3}, {"a", 1}, {"b", 5}, {"b", 2}}

	groups := make(map[string]int)
	for _, it := range items {
		groups[it.Cat] += it.Val
	}

	var results []Result
	for cat, total := range groups {
		results = append(results, Result{cat, total})
	}

	sort.Slice(results, func(i, j int) bool { return results[i].Total > results[j].Total })

	fmt.Println(results)
}
