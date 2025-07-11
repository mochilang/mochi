//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var x int = 12
	_ = x
	var msg string = func() string {
		if x > 10 {
			return "yes"
		} else {
			return "no"
		}
	}()
	fmt.Println(_sprint(msg))
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
