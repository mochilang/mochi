//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

func main() {
	target := 269696
	modulus := 1000000
	n := 1
	for {
		square := (n * n)
		ending := (square % modulus)
		if ending == target {
			fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("The smallest number whose square ends with "+"269696"+" is "+fmt.Sprint(any(n)))), "\n"))
			break
		}
		n = (n + 1)
	}
}
