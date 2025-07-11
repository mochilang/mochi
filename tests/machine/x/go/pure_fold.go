//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func triple(x int) int {
	return (x * 3)
}

func main() {
	fmt.Println(_sprint(triple((1 + 2))))
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
