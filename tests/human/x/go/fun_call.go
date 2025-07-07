//go:build ignore

package main

import "fmt"

func add(a int, b int) int {
	return a + b
}

func main() {
	fmt.Println(add(2, 3))
}
