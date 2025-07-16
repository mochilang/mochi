package main

import (
	"fmt"
)

// line 1
func outer(x int) int {
	// line 2
	var inner = func(y int) int {
		return (x + y)
	}
	return inner(5)
}

func main() {
	fmt.Println(outer(3))
}
