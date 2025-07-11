//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

func main() {
	var s string = "mochi"
	fmt.Println(_fmt(_indexString(s, 1)))
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

func _indexString(s string, i int) string {
	runes := []rune(s)
	if i < 0 {
		i += len(runes)
	}
	if i < 0 || i >= len(runes) {
		panic("index out of range")
	}
	return string(runes[i])
}
