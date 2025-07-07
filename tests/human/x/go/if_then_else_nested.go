package main

import (
	"fmt"
)

func main() {
	var x int = 8
	_ = x
	var msg string = func() string {
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
