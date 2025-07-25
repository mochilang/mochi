//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 1
func fib(n int) int {
	if n < 2 {
		return n
	}
	return (fib((n - 1)) + fib((n - 2)))
}

// line 6
func main() {
	i := -1
	for i <= 10 {
		if i < 0 {
			fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("fib("+fmt.Sprint(any(i))+") returned error: negative n is forbidden")), "\n"))
		} else {
			fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("fib("+fmt.Sprint(any(i))+") = "+fmt.Sprint(any(fib(i))))), "\n"))
		}
		i = (i + 1)
	}
}

func main() {
	main()
}
