//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var matrix [][]int = [][]int{[]int{1, 2}, []int{3, 4}}
	matrix[1][0] = 5
	fmt.Println(_sprint(matrix[1][0]))
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
