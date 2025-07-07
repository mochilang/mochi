//go:build ignore

package main

import "fmt"

func boom() bool {
	fmt.Println("boom")
	return true
}

func main() {
	fmt.Println((1 < 2) && (2 < 3) && (3 < 4))
	fmt.Println((1 < 2) && (2 > 3) && boom())
	fmt.Println((1 < 2) && (2 < 3) && (3 > 4) && boom())
}
