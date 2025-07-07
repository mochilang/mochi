//go:build ignore

package main

import (
	"fmt"
	"sort"
)

type Data struct {
	A int
	B int
}

func main() {
	data := []Data{{1, 2}, {1, 1}, {0, 5}}

	sort.Slice(data, func(i, j int) bool {
		if data[i].A == data[j].A {
			return data[i].B < data[j].B
		}
		return data[i].A < data[j].A
	})

	fmt.Println(data)
}
