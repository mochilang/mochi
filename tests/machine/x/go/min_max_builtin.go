//go:build ignore

package main

import (
	"fmt"
	"reflect"

	"golang.org/x/exp/constraints"
)

func main() {
	var nums []int = []int{3, 1, 4}
	fmt.Println(_sprint(_minOrdered[int](nums)))
	fmt.Println(_sprint(_maxOrdered[int](nums)))
}

func _maxOrdered[T constraints.Ordered](s []T) T {
	if len(s) == 0 {
		var zero T
		return zero
	}
	m := s[0]
	for _, v := range s[1:] {
		if v > m {
			m = v
		}
	}
	return m
}

func _minOrdered[T constraints.Ordered](s []T) T {
	if len(s) == 0 {
		var zero T
		return zero
	}
	m := s[0]
	for _, v := range s[1:] {
		if v < m {
			m = v
		}
	}
	return m
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
