//go:build ignore

package main

import "fmt"

func main() {
	m := map[int]string{1: "a", 2: "b"}
	_, ok := m[1]
	fmt.Println(ok)
	_, ok = m[3]
	fmt.Println(ok)
}
