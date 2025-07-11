//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var m map[int]string = map[int]string{1: "a", 2: "b"}
	key0 := 1
	m1 := m
	_, ok2 := m1[key0]
	fmt.Println(ok2)
	key3 := 3
	m4 := m
	_, ok5 := m4[key3]
	fmt.Println(ok5)
}
