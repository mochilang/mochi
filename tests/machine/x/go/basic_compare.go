//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var a int = (10 - 3)
	var b int = (2 + 2)
	fmt.Println(_sprint(a))
	fmt.Println(_sprint((a == 7)))
	fmt.Println(_sprint((b < 5)))
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
