//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"sort"
)

type DataItem struct {
	Tag string `json:"tag"`
	Val int    `json:"val"`
}

func main() {
	dataItems := []DataItem{
		{Tag: "a", Val: 1},
		{Tag: "a", Val: 2},
		{Tag: "b", Val: 3},
	}

	groups := map[string]*data.Group{}
	order := []string{}
	for _, d := range dataItems {
		key := d.Tag
		g, ok := groups[key]
		if !ok {
			g = &data.Group{Key: key}
			groups[key] = g
			order = append(order, key)
		}
		g.Items = append(g.Items, d)
	}

	tmp := []map[string]any{}
	for _, key := range order {
		g := groups[key]
		total := 0
		for _, item := range g.Items {
			total += item.(DataItem).Val
		}
		tmp = append(tmp, map[string]any{"tag": g.Key, "total": total})
	}

	sort.Slice(tmp, func(i, j int) bool {
		return fmt.Sprint(tmp[i]["tag"]) < fmt.Sprint(tmp[j]["tag"])
	})

	for i, r := range tmp {
		if i > 0 {
			fmt.Print(" ")
		}
		fmt.Print(r)
	}
	fmt.Println()
}
