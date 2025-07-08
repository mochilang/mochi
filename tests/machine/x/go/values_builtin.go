//go:build ignore

package main

import (
    "fmt"
)

func main() {
    m := map[string]interface{}{"a": 1, "b": 2, "c": 3}
    fmt.Println(values(m))
}
