//go:build ignore

package main

import (
	"fmt"
)

// line 1
func sum_rec(n int, acc int) int {
	if n == 0 {
		return acc
	}
	return sum_rec((n - 1), (acc + n))
}

func main() {
	fmt.Println(sum_rec(10, 0))
}
