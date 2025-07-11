//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var s string = "mochi"
	fmt.Println(string([]rune(s)[1]))
}
