//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/ffi/python"
	"reflect"
)

func main() {
	fmt.Println(_fmt(func() any { v, _ := python.Attr("math", "sqrt", 16.0); return v }()))
	fmt.Println(_fmt(func() any { v, _ := python.Attr("math", "pi"); return v }()))
}

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
	}
	return fmt.Sprint(v)
}
