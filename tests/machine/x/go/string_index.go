//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var s string = "mochi"
	fmt.Println(_sprint(_indexString(s, 1)))
}

func _indexString(s string, i int) string {
	runes := []rune(s)
	if i < 0 {
		i += len(runes)
	}
	if i < 0 || i >= len(runes) {
		panic("index out of range")
	}
	return string(runes[i])
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
