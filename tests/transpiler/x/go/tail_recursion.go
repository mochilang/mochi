//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:42 GMT+7
package main

import (
	"fmt"
)

func sum_rec(n int, acc int) int {
	if n == 0 {
		return acc
	}
	return sum_rec((n - 1), (acc + n))
}

func main() {
	fmt.Println(sum_rec(10, 0))
}
