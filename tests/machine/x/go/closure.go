//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func makeAdder(n int) func(int) int {
	return func(x int) int {
		return (x + n)
	}
}

func main() {
	var add10 func(int) int = makeAdder(10)
	fmt.Println(_sprint(add10(7)))
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
