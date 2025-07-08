//go:build ignore

package main

import (
    "fmt"
)

func main() {
    square := func(x int) int {        return x * x
    }
    fmt.Println(square(6))
}
