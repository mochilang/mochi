//go:build ignore

package main

import (
	"fmt"
	"slices"
)

func main() {
	var nums []int = []int{1, 2, 3}
	fmt.Println(slices.Contains(nums, 2))
	fmt.Println(slices.Contains(nums, 4))
}
