//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	type M struct {
		A int `json:"a"`
		B int `json:"b"`
		C int `json:"c"`
	}

	var m M = M{
		A: 1,
		B: 2,
		C: 3,
	}
	fmt.Println(_values(m))
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
