//go:build ignore

package main

import (
    "fmt"
)

func main() {
x := 8
msg := (func() interface{} { if x > 10 { return "big" } else { return (func() interface{} { if x > 5 { return "medium" } else { return "small" } })() } })()
fmt.Println(msg)
}
