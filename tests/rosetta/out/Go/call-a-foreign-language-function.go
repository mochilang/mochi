//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 6
func strdup(s string) string {
	return s + ""
}

// line 11
func main() {
	go1 := "hello C"
	c2 := strdup(go1)
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(c2)), "\n"))
}

func main() {
	main()
}
