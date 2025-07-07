//go:build ignore

package main

import (
    "fmt"
)

func add(a int, b int) int {
    return a + b
}


func main() {
    add5 := add(5)
    fmt.Println(add5(3))
}
