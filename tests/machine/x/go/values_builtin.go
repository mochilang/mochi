//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	m := M{
		1,
		2,
		3,
	}
	_print(_values(m))
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		if a == nil {
			fmt.Print("<nil>")
			continue
		}
		rv := reflect.ValueOf(a)
		if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
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
