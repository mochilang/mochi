//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func containsInt(slice []int, val int) bool {
	for _, x := range slice {
		if x == val {
			return true
		}
	}
	return false
}

func main() {
	xs := []int{1, 2, 3}
	var ys []int
	for _, x := range xs {
		if x%2 == 1 {
			ys = append(ys, x)
		}
	}
	fmt.Println(containsInt(ys, 1))
	fmt.Println(containsInt(ys, 2))

	m := map[string]int{"a": 1}
	_, ok := m["a"]
	fmt.Println(ok)
	_, ok = m["b"]
	fmt.Println(ok)

	s := "hello"
	fmt.Println(strings.Contains(s, "ell"))
	fmt.Println(strings.Contains(s, "foo"))
}
