//go:build ignore

package main

import (
	"encoding/json"
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
	fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("Circle area with r ="), _fmt(r), _fmt("=>"), _fmt(area)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("Square root of 49:"), _fmt(root)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("sin(π/4):"), _fmt(sin45)}, " "), " "))
	fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("log(e):"), _fmt(log_e)}, " "), " "))
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
