//go:build ignore

package main

import "fmt"

func main() {
	x := 8
	var msg string
	if x > 10 {
		msg = "big"
	} else if x > 5 {
		msg = "medium"
	} else {
		msg = "small"
	}
	fmt.Println(msg)
}
