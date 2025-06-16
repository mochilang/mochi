package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func deleteDuplicates(nums []int) []int {
	var n int = len(nums)
	if (n == 0) {
		return _cast[[]int]([]any{})
	}
	var result []int = []int{nums[0]}
	var i int = 1
	for (i < n) {
		if (nums[i] != result[(len(result) - 1)]) {
			result = append(append([]int{}, result...), []int{nums[i]}...)
		}
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect(_equal(deleteDuplicates([]int{1, 1, 2}), []int{1, 2}))
}

func example_2() {
	expect(_equal(deleteDuplicates([]int{1, 1, 2, 3, 3}), []int{1, 2, 3}))
}

func empty_list() {
	expect(_equal(deleteDuplicates([]int{}), []any{}))
}

func single_element() {
	expect(_equal(deleteDuplicates([]int{0}), []int{0}))
}

func main() {
	example_1()
	example_2()
	empty_list()
	single_element()
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

