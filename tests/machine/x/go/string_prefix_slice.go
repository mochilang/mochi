//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var prefix string = "fore"
	var s1 string = "forest"
	fmt.Println(_sprint((_sliceString(s1, 0, 4) == prefix)))
	var s2 string = "desert"
	fmt.Println(_sprint((_sliceString(s2, 0, 4) == prefix)))
}

func _sliceString(s string, i, j int) string {
	start := i
	end := j
	n := len([]rune(s))
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if end < start {
		end = start
	}
	return string([]rune(s)[start:end])
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
