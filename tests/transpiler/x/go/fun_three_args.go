//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:28 GMT+7
package main

import (
	"fmt"
)

func sum3(a int, b int, c int) int {
	return ((a + b) + c)
}

func main() {
	fmt.Println(sum3(1, 2, 3))
}
