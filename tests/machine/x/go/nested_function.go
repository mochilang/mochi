//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// line 1
func outer(x int) int {
	// line 2
	var inner = func(y int) int {
		return (x + y)
	}
	return inner(5)
}

func main() {
	fmt.Println(_fmt(outer(3)))
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
