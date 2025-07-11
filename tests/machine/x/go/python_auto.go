//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/ffi/python"
	"reflect"
)

func main() {
	fmt.Println(_sprint(func() any { v, _ := python.Attr("math", "sqrt", 16.0); return v }()))
	fmt.Println(_sprint(func() any { v, _ := python.Attr("math", "pi"); return v }()))
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
