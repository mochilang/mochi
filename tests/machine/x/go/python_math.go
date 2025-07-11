//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/ffi/python"
	"reflect"
	"strings"
)

func main() {
	var r float64 = 3.0
	var area float64 = (func() float64 { v, _ := python.Attr("math", "pi"); return v.(float64) }() * func() float64 { v, _ := python.Attr("math", "pow", r, 2.0); return v.(float64) }())
	var root float64 = func() float64 { v, _ := python.Attr("math", "sqrt", 49.0); return v.(float64) }()
	var sin45 float64 = func() float64 {
		v, _ := python.Attr("math", "sin", (func() float64 { v, _ := python.Attr("math", "pi"); return v.(float64) }() / 4.0))
		return v.(float64)
	}()
	var log_e float64 = func() float64 {
		v, _ := python.Attr("math", "log", func() float64 { v, _ := python.Attr("math", "e"); return v.(float64) }())
		return v.(float64)
	}()
	fmt.Println(strings.TrimRight(strings.Join([]string{_sprint("Circle area with r ="), _sprint(r), _sprint("=>"), _sprint(area)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_sprint("Square root of 49:"), _sprint(root)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_sprint("sin(π/4):"), _sprint(sin45)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_sprint("log(e):"), _sprint(log_e)}, " "), " "))
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
