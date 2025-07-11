//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var a []int = []int{1, 2}
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(_sprint(append(_convSlice[int, any](a), 3)), "["), "]"))
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
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
