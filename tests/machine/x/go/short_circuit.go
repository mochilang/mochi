package main

import (
	"fmt"
)

// line 1
func boom(a int, b int) bool {
	fmt.Println("boom")
	return true
}

func main() {
	fmt.Println((false && boom(1, 2)))
	fmt.Println((true || boom(1, 2)))
}
