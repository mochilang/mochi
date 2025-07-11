//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 1
func sum_rec(n int, acc int) int {
	if n == 0 {
		return acc
	}
	return sum_rec((n - 1), (acc + n))
}

func main() {
	fmt.Println(_sprint(sum_rec(10, 0)))
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
