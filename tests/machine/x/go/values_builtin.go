//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var m map[string]int = map[string]int{
		"a": 1,
		"b": 2,
		"c": 3,
	}
	fmt.Println(values(m))
}
