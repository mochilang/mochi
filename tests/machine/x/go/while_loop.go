//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var i int = 0
	for {
		if !(i < 3) {
			break
		}
		fmt.Println(_sprint(i))
		i = (i + 1)
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
