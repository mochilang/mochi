//go:build ignore

package main

import (
	"fmt"
	"slices"
)

func main() {
	nums := []int{1, 2, 3}
	fmt.Println(slices.Contains(nums, 2))
	fmt.Println(slices.Contains(nums, 4))
}
