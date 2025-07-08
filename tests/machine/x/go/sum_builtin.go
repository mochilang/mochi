//go:build ignore

package main

import (
    "fmt"
)

func main() {
    fmt.Println(func() int { s := 0; for _, n := range []int{1, 2, 3} { s += n }; return s }())
}
