//go:build ignore

package main

import (
    "fmt"
)

func main() {
    s := "catch"
    fmt.Println("cat" in s)
    fmt.Println("dog" in s)
}
