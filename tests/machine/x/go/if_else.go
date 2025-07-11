//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var x int = 5
	if x > 3 {
		fmt.Println(_sprint("big"))
	} else {
		fmt.Println(_sprint("small"))
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
