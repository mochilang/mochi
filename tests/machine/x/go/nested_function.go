//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func outer(x int) int {
	// line 2
	var inner = func(y int) int {
		return (x + y)
	}
	return inner(5)
}

func main() {
	fmt.Println(_sprint(outer(3)))
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
