//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 4
func bar(a int, b int, c int) {
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(a))+", "+fmt.Sprint(any(b))+", "+fmt.Sprint(any(c)))), "\n"))
}

// line 8
func main() {
	var args map[string]int = map[string]int{}
	args["a"] = 3
	args["b"] = 2
	args["c"] = 1
	bar(args["a"], args["b"], args["c"])
}

func main() {
	main()
}
