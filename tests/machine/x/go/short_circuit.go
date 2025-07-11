//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func boom(a int, b int) bool {
	fmt.Println(_sprint("boom"))
	return true
}

func main() {
	fmt.Println(_sprint((false && boom(1, 2))))
	fmt.Println(_sprint((true || boom(1, 2))))
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
