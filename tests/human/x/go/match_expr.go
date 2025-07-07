//go:build ignore

package main

import "fmt"

func main() {
	x := 2
	var label string
	switch x {
	case 1:
		label = "one"
	case 2:
		label = "two"
	case 3:
		label = "three"
	default:
		label = "unknown"
	}
	fmt.Println(label)
}
