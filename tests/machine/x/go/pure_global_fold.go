//go:build ignore

package main

import (
    "fmt"
)

func inc(x int) int {
    return x + k
}


func main() {
    k := 2
    fmt.Println(inc(3))
}
