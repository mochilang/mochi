//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 1
func outer(x int) int {
	// line 2
	var inner = func(y int) int {
		return (x + y)
	}
	return inner(5)
}

func main() {
	fmt.Println(strings.TrimSpace(fmt.Sprintln(any(outer(3)))))
}
