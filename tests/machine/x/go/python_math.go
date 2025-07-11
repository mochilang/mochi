//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/ffi/python"
)

func main() {
	r := 3.0
	area := (func() float64 { v, _ := python.Attr("math", "pi"); return v.(float64) }() * func() float64 { v, _ := python.Attr("math", "pow", r, 2.0); return v.(float64) }())
	root := func() float64 { v, _ := python.Attr("math", "sqrt", 49.0); return v.(float64) }()
	sin45 := func() float64 {
		v, _ := python.Attr("math", "sin", (func() float64 { v, _ := python.Attr("math", "pi"); return v.(float64) }() / 4.0))
		return v.(float64)
	}()
	log_e := func() float64 {
		v, _ := python.Attr("math", "log", func() float64 { v, _ := python.Attr("math", "e"); return v.(float64) }())
		return v.(float64)
	}()
	fmt.Println("Circle area with r =", r, "=>", area)
	fmt.Println("Square root of 49:", root)
	fmt.Println("sin(π/4):", sin45)
	fmt.Println("log(e):", log_e)
}
