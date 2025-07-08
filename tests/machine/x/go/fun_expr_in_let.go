package main

import (
	"fmt"
)

func main() {
	var square func(int) int = func(x int) int {
		return (x * x)
	}
	fmt.Println(square(6))
}
