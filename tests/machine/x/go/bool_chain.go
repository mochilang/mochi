//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// line 1
func boom() bool {
	fmt.Println(_fmt("boom"))
	return true
}

func main() {
	fmt.Println(_fmt((((1 < 2) && (2 < 3)) && (3 < 4))))
	fmt.Println(_fmt((((1 < 2) && (2 > 3)) && boom())))
	fmt.Println(_fmt(((((1 < 2) && (2 < 3)) && (3 > 4)) && boom())))
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
