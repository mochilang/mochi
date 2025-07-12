//go:build ignore

package main

import (
	"fmt"
	"slices"
)

func main() {
	xs := []int{1, 2, 3}
	fmt.Println(slices.Contains(xs, 2))
	fmt.Println(!(slices.Contains(xs, 5)))
}
