//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	for i := 1; i < 4; i++ {
		fmt.Println(_sprint(i))
	}
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
