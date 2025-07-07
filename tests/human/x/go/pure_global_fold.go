//go:build ignore

package main

import "fmt"

var k = 2

func inc(x int) int {
    return x + k
}

func main() {
    fmt.Println(inc(3))
}
