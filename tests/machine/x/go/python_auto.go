//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/ffi/python"
)

func main() {
	fmt.Println(func() any { v, _ := python.Attr("math", "sqrt", 16.0); return v }())
	fmt.Println(func() any { v, _ := python.Attr("math", "pi"); return v }())
}
