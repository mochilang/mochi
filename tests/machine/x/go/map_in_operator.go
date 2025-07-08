//go:build ignore

package main

import (
    "fmt"
)

func main() {
    m := map[int]string{1: "a", 2: "b"}
    fmt.Println(func() bool { _, ok := m[1]; return ok }())
    fmt.Println(func() bool { _, ok := m[3]; return ok }())
}
