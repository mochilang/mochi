//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:24 GMT+7
//
package main

import (
	"fmt"
)

var numbers []int = []int{1, 2, 3, 4, 5, 6, 7, 8, 9}

func main() {
	for _, n := range numbers {
		fmt.Println(n)
	}
}
