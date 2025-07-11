//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
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
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_fmt(_values(m)), "["), "]"))
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
