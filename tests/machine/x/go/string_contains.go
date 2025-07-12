//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "catch"
	_ = s
	fmt.Println(strings.Contains(s, "cat"))
	fmt.Println(strings.Contains(s, "dog"))
}
