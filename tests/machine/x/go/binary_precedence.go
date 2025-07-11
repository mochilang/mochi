//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	fmt.Println(_sprint((1 + (2 * 3))))
	fmt.Println(_sprint(((1 + 2) * 3)))
	fmt.Println(_sprint(((2 * 3) + 1)))
	fmt.Println(_sprint((2 * (3 + 1))))
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
