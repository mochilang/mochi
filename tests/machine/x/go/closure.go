//go:build ignore

package main

import (
	"fmt"
)

// line 1
func makeAdder(n int) func(int) int {
	return func(x int) int {
		return (x + n)
	}
}

func main() {
	add10 := makeAdder(10)
	fmt.Println(add10(7))
}
