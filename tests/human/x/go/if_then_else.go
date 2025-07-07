//go:build ignore

package main

import "fmt"

func main() {
	x := 12
	var msg string
	if x > 10 {
		msg = "yes"
	} else {
		msg = "no"
	}
	fmt.Println(msg)
}
