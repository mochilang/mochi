//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var s string = "catch"
	fmt.Println(_sprint(strings.Contains(s, "cat")))
	fmt.Println(_sprint(strings.Contains(s, "dog")))
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
