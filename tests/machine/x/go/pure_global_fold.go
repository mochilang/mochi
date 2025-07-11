//go:build ignore

package main

import (
	"fmt"
)

// line 2
func inc(x int) int {
	return (x + k)
}

var k int

func main() {
	k = 2
	fmt.Println(inc(3))
}
