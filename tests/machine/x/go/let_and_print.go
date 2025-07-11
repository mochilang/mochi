//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var a int = 10
	var b int = 20
	fmt.Println(_sprint((a + b)))
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
