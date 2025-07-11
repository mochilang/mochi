//go:build ignore

package main

import (
	"fmt"
	"reflect"

	"golang.org/x/exp/constraints"
)

func main() {
	fmt.Println(_sprint(_sumOrdered[int]([]int{1, 2, 3})))
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

func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum
}
