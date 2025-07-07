//go:build ignore

package main

import (
    "fmt"
)

func triple(x int) int {
    return x * 3
}


func main() {
    fmt.Println(triple(1 + 2))
}
