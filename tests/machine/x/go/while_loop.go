//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var i int = 0
	for {
		if !(i < 3) {
			break
		}
		fmt.Println(i)
		i = (i + 1)
	}
}
