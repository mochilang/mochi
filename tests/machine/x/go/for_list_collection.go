//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	for _, n := range []int{1, 2, 3} {
		fmt.Println(_sprint(n))
	}
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
