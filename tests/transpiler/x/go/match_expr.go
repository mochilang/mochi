//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:36 GMT+7
package main

import (
	"fmt"
)

var x int = 2

var label string = func() string {
	if x == 1 {
		return "one"
	} else {
		return func() string {
			if x == 2 {
				return "two"
			} else {
				return func() string {
					if x == 3 {
						return "three"
					} else {
						return "unknown"
					}
				}()
			}
		}()
	}
}()

func main() {
	fmt.Println(label)
}
