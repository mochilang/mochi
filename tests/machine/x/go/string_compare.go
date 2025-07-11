//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	fmt.Println(_sprint(("a" < "b")))
	fmt.Println(_sprint(("a" <= "a")))
	fmt.Println(_sprint(("b" > "a")))
	fmt.Println(_sprint(("b" >= "b")))
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
