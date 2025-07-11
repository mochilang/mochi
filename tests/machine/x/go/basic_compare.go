//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

func main() {
	var a int = (10 - 3)
	var b int = (2 + 2)
	fmt.Println(_fmt(a))
	fmt.Println(_fmt((a == 7)))
	fmt.Println(_fmt((b < 5)))
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
