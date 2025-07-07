//go:build ignore

package main

import "fmt"

func outer(x int) int {
	inner := func(y int) int {
		return x + y
	}
	return inner(5)
}

func main() {
	fmt.Println(outer(3))
}
