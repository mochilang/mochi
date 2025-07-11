//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 2
func inc(x int) int {
	return (x + k)
}

var k int

func main() {
	k = 2
	fmt.Println(_sprint(inc(3)))
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
