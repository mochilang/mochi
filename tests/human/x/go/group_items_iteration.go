package main

import (
	"fmt"
	"sort"
)

type Item struct {
	Tag string
	Val int
}

func main() {
	data := []Item{{"a", 1}, {"a", 2}, {"b", 3}}

	groupMap := make(map[string][]Item)
	var order []string
	for _, d := range data {
		if _, exists := groupMap[d.Tag]; !exists {
			order = append(order, d.Tag)
		}
		groupMap[d.Tag] = append(groupMap[d.Tag], d)
	}

	var tmp []map[string]interface{}
	for _, tag := range order {
		total := 0
		for _, x := range groupMap[tag] {
			total += x.Val
		}
		tmp = append(tmp, map[string]interface{}{"tag": tag, "total": total})
	}

	sort.Slice(tmp, func(i, j int) bool {
		return tmp[i]["tag"].(string) < tmp[j]["tag"].(string)
	})

	if len(tmp) > 0 {
		// Print each map separated by space
		for i, m := range tmp {
			if i > 0 {
				fmt.Print(" ")
			}
			fmt.Print(m)
		}
	}
}
