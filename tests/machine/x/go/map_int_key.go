//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var m map[int]string = map[int]string{1: "a", 2: "b"}
	fmt.Println(m[1])
}
