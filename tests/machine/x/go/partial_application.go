//go:build ignore

package main

import (
    "fmt"
)

func add(a int, b int) int {
    return a + b
}


func main() {
    add5 := func(b int) int { return add(5, b) }
    fmt.Println(add5(3))
}
