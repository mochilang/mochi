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
	var add5 int = add(5)
	fmt.Println(add5(3))
}
