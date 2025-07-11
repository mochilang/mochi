//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var m map[int]string = map[int]string{1: "a", 2: "b"}
	fmt.Println(_sprint(m[1]))
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
