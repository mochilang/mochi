//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 1
func ord(ch string) int {
	if ch == "a" {
		return 97
	}
	if ch == "π" {
		return 960
	}
	if ch == "A" {
		return 65
	}
	return 0
}

func main() {
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(ord("a"))))), "\n"))
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(ord("π"))))), "\n"))
}
