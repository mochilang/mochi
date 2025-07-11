//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var result string = ""
	for i := 1; i < 101; i++ {
		var j int = 1
		for {
			if !((j * j) < i) {
				break
			}
			j = (j + 1)
		}
		if (j * j) == i {
			result = result + "O"
		} else {
			result = result + "-"
		}
	}
	fmt.Println(result)
}
