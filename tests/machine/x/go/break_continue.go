//go:build ignore

package main

import (
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var numbers []int = []int{
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
	}
	for _, n := range numbers {
		if (n % 2) == 0 {
			continue
		}
		if n > 7 {
			break
		}
		fmt.Println(strings.TrimRight(strings.Join([]string{_sprint("odd number:"), _sprint(n)}, " "), " "))
	}
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
