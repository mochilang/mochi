//go:build ignore

package main

import (
	"fmt"
)

func main() {
	x := 12
	_ = x
	msg := func() string {
		if x > 10 {
			return "yes"
		} else {
			return "no"
		}
	}()
	fmt.Println(msg)
}
