//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var m map[string]int = map[string]int{"a": 1, "b": 2}
	for k := range m {
		fmt.Println(k)
	}
}
