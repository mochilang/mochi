//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// line 1
func twoSum(nums []int, target int) []int {
	var n int = len(nums)
	for i := 0; i < n; i++ {
		for j := (i + 1); j < n; j++ {
			if (nums[i] + nums[j]) == target {
				return []int{i, j}
			}
		}
	}
	return []int{-1, -1}
}

func main() {
	var result []int = twoSum([]int{
		2,
		7,
		11,
		15,
	}, 9)
	fmt.Println(_fmt(result[0]))
	fmt.Println(_fmt(result[1]))
}

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
	}
	return fmt.Sprint(v)
}
