//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:37 GMT+7
package main

import (
	"fmt"
)

func outer(x int) int {
	inner := func(y int) int {
		return (x + y)
	}
	return inner(5)
}

func main() {
	fmt.Println(outer(3))
}
