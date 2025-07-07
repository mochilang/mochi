//go:build ignore

package main

import "fmt"

func main() {
	m := map[string]int{"a": 1, "b": 2}
	_, ok := m["a"]
	fmt.Println(ok)
	_, ok = m["c"]
	fmt.Println(ok)
}
