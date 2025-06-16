package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func swapPairs(nums []int) []int {
	var i int = 0
	var result []any = []any{}
	for (i < len(nums)) {
		if ((i + 1) < len(nums)) {
			result = append(append([]any{}, result...), _toAnySlice([]int{nums[(i + 1)], nums[i]})...)
		} else {
			result = append(append([]any{}, result...), _toAnySlice([]int{nums[i]})...)
		}
		i = (i + 2)
	}
	return _cast[[]int](result)
}

func example_1() {
	expect(_equal(swapPairs([]int{1, 2, 3, 4}), []int{2, 1, 4, 3}))
}

func example_2() {
	expect(_equal(swapPairs([]int{}), []any{}))
}

func example_3() {
	expect(_equal(swapPairs([]int{1}), []int{1}))
}

func odd_length() {
	expect(_equal(swapPairs([]int{1, 2, 3}), []int{2, 1, 3}))
}

func main() {
	example_1()
	example_2()
	example_3()
	odd_length()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

func _toAnySlice[T any](s []T) []any {
    out := make([]any, len(s))
    for i, v := range s { out[i] = v }
    return out
}

