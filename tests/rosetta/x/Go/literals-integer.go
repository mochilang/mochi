//go:build ignore
// +build ignore

package main

import "fmt"

func main() {
	fmt.Println(727 == 0x2d7)         // prints true
	fmt.Println(727 == 01327)         // prints true
	fmt.Println(727 == 0b10110_10111) // prints true
	fmt.Println(727 == '˗')           // prints true
}
