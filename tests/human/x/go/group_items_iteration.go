//go:build ignore

package main

import (
	"fmt"
	"sort"
)

type Item struct {
	Tag string
	Val int
}

type GroupResult struct {
	Tag   string
	Total int
}

func main() {
	data := []Item{{"a", 1}, {"a", 2}, {"b", 3}}

	groups := make(map[string][]Item)
	for _, d := range data {
		groups[d.Tag] = append(groups[d.Tag], d)
	}

	var tmp []GroupResult
	for tag, items := range groups {
		total := 0
		for _, it := range items {
			total += it.Val
		}
		tmp = append(tmp, GroupResult{Tag: tag, Total: total})
	}

	sort.Slice(tmp, func(i, j int) bool {
		return tmp[i].Tag < tmp[j].Tag
	})

	fmt.Println(tmp)
}
