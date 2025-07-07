//go:build ignore

package main

import "fmt"

func main() {
	m := map[string]int{"a": 1, "b": 2, "c": 3}
	vals := []int{}
	for _, v := range m {
		vals = append(vals, v)
	}
	fmt.Println(vals)
}
