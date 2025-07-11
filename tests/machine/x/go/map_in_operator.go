//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

func main() {
	var m map[int]string = map[int]string{1: "a", 2: "b"}
	_tmp0 := 1
	_tmp1 := m
	_, _tmp2 := _tmp1[_tmp0]
	fmt.Println(_fmt(_tmp2))
	_tmp3 := 3
	_tmp4 := m
	_, _tmp5 := _tmp4[_tmp3]
	fmt.Println(_fmt(_tmp5))
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
