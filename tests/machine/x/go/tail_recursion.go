//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// line 1
func sum_rec(n int, acc int) int {
	if n == 0 {
		return acc
	}
	return sum_rec((n - 1), (acc + n))
}

func main() {
	fmt.Println(_fmt(sum_rec(10, 0)))
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
