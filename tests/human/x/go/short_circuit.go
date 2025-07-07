//go:build ignore

package main

import "fmt"

func boom(a, b int) bool {
	fmt.Println("boom")
	return true
}

func main() {
	fmt.Println(false && boom(1, 2))
	fmt.Println(true || boom(1, 2))
}
