//go:build ignore

package main

import (
    "fmt"
)

func main() {
    m := map[string]interface{}{"a": 1, "b": 2}
    for _, k := range m {
        fmt.Println(k)
    }
}
