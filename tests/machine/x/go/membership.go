//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"slices"
)

func main() {
	var nums []int = []int{1, 2, 3}
	fmt.Println(_sprint(slices.Contains(nums, 2)))
	fmt.Println(_sprint(slices.Contains(nums, 4)))
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
