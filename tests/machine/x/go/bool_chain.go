//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func boom() bool {
	fmt.Println(_sprint("boom"))
	return true
}

func main() {
	fmt.Println(_sprint((((1 < 2) && (2 < 3)) && (3 < 4))))
	fmt.Println(_sprint((((1 < 2) && (2 > 3)) && boom())))
	fmt.Println(_sprint(((((1 < 2) && (2 < 3)) && (3 > 4)) && boom())))
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
