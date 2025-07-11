//go:build ignore

package main

import (
	"fmt"
)

func main() {
	fmt.Println([]int{1, 2, 3}[1:3])
	fmt.Println([]int{1, 2, 3}[0:2])
	fmt.Println(string([]rune("hello")[1:4]))
}
