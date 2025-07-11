//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func sum3(a int, b int, c int) int {
	return ((a + b) + c)
}

func main() {
	fmt.Println(_sprint(6))
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
