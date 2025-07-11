//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var numbers []int = []int{
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
	}
	for _, n := range numbers {
		if (n % 2) == 0 {
			continue
		}
		if n > 7 {
			break
		}
		fmt.Println(strings.TrimRight(strings.Join([]string{_fmt("odd number:"), _fmt(n)}, " "), " "))
	}
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
