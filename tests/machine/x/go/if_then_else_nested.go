//go:build ignore

package main

import (
	"fmt"
)

func main() {
	x := 8
	_ = x
	msg := func() string {
		if x > 10 {
			return "big"
		} else {
			return func() string {
				if x > 5 {
					return "medium"
				} else {
					return "small"
				}
			}()
		}
	}()
	fmt.Println(msg)
}
