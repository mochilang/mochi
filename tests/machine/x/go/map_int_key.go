//go:build ignore

package main

import (
	"fmt"
)

func main() {
	m := map[interface{}]interface{}{1: "a", 2: "b"}
	fmt.Println(m[1])
}
