//go:build ignore

package main

import (
	"fmt"
	"sort"
)

type item struct {
	n int
	v string
}

func main() {
	items := []item{{1, "a"}, {1, "b"}, {2, "c"}}
	sort.SliceStable(items, func(i, j int) bool { return items[i].n < items[j].n })
	var result []string
	for _, it := range items {
		result = append(result, it.v)
	}
	fmt.Println(result)
}
