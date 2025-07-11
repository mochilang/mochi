//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var square func(int) int = func(x int) int {
		return (x * x)
	}
	fmt.Println(_sprint(square(6)))
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
