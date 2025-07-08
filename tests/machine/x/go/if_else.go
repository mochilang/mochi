//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var x int = 5
	if x > 3 {
		fmt.Println("big")
	} else {
		fmt.Println("small")
	}
}
