//go:build ignore

package main

import (
    "fmt"
)

func sum3(a int, b int, c int) int {
    return a + b + c
}


func main() {
    fmt.Println(sum3(1, 2, 3))
}
