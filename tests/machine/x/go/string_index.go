//go:build ignore

package main

import (
	"fmt"
)

func main() {
	s := "mochi"
	fmt.Println(string([]rune(s)[1]))
}
