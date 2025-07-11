//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var _data []int = []int{1, 2}
	var flag bool = len(func() []int {
		_res := []int{}
		for _, x := range _data {
			if x == 1 {
				if x == 1 {
					_res = append(_res, x)
				}
			}
		}
		return _res
	}()) > 0
	fmt.Println(_sprint(flag))
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
