//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func add(a int, b int) int {
	return (a + b)
}

func main() {
	var add5 int = func(p0 int) int { return add(5, p0) }
	fmt.Println(_sprint(add5(3)))
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
