//go:build ignore

package main

import (
	"fmt"
)

// line 1
func add(a int, b int) int {
	return (a + b)
}

func main() {
	var add5 int = func(p0 int) int { return add(5, p0) }
	fmt.Println(add5(3))
}
