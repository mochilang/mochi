//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// line 1
func makeAdder(n int) func(int) int {
	return func(x int) int {
		return (x + n)
	}
}

func main() {
	var add10 func(int) int = makeAdder(10)
	fmt.Println(_fmt(add10(7)))
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
