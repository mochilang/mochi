//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var m map[string]int = map[string]int{
		"a": 1,
		"b": 2,
		"c": 3,
	}
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint(_values(m)), "["), "]"))
}

func _values(v any) []any {
	switch m := v.(type) {
	case map[string]any:
		res := make([]any, 0, len(m))
		for _, vv := range m {
			res = append(res, vv)
		}
		return res
	case map[any]any:
		res := make([]any, 0, len(m))
		for _, vv := range m {
			res = append(res, vv)
		}
		return res
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Struct {
		n := rv.NumField()
		res := make([]any, 0, n)
		for i := 0; i < n; i++ {
			res = append(res, rv.Field(i).Interface())
		}
		return res
	}
	panic("values() expects map")
}
