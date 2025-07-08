//go:build ignore

package main

import (
    "fmt"
)

func main() {
    x := 3
    y := 4
    m := map[string]interface{}{"a": x, "b": y}
    fmt.Println(m["a"], m["b"])
}
