//go:build ignore

package main

import (
    "fmt"
    "strconv"
)

func main() {
    fmt.Println(func() int { v, _ := strconv.Atoi("1995"); return v }())
}
