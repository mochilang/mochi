//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

func main() {
	s := "mochi"
	fmt.Println(strings.TrimSpace(fmt.Sprintln(any(string([]rune(s)[1])))))
}
