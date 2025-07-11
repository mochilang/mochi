//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_sprint([]int{1, 2, 3}[1:3]), "["), "]"))
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_sprint([]int{1, 2, 3}[0:2]), "["), "]"))
	fmt.Println(_sprint(_sliceString("hello", 1, 4)))
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
