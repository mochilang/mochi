//go:build ignore

package main

import "fmt"

func main() {
	x := 1 + 2
	if x != 3 {
		panic(fmt.Sprintf("expected 3, got %d", x))
	}
	fmt.Println("ok")
}
