//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

func main() {
	m := map[int]string{1: "a", 2: "b"}
	fmt.Println(strings.TrimSpace(fmt.Sprintln(any(m[1]))))
}
