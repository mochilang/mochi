package main

import (
	"fmt"
)

// line 1
func boom() bool {
	fmt.Println("boom")
	return true
}

func main() {
	fmt.Println((((1 < 2) && (2 < 3)) && (3 < 4)))
	fmt.Println((((1 < 2) && (2 > 3)) && boom()))
	fmt.Println(((((1 < 2) && (2 < 3)) && (3 > 4)) && boom()))
}
