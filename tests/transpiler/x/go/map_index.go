//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:35 GMT+7
package main

import (
	"fmt"
)

var m map[string]int = map[string]int{"a": 1, "b": 2}

func main() {
	fmt.Println(m["b"])
}
