//go:build ignore

// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
)

type v map[string]any

// line 1
func bottles(n int) string {
	if n == 0 {
		return "No more bottles"
	}
	if n == 1 {
		return "1 bottle"
	}
	return fmt.Sprint(any(n)) + " bottles"
}

// line 11
func mainFn() {
	var i int = 99
	for i > 0 {
		fmt.Println(any(bottles(i) + " of beer on the wall"))
		fmt.Println(any(bottles(i) + " of beer"))
		fmt.Println(any("Take one down, pass it around"))
		fmt.Println(any(bottles((i - 1)) + " of beer on the wall"))
		i = (i - 1)
	}
}

func main() {
	mainFn()
}
