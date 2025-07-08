//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var x int = 12
	_ = x
	var msg string = func() string {
		if x > 10 {
			return "yes"
		} else {
			return "no"
		}
	}()
	fmt.Println(msg)
}
