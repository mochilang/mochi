package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func twoSum(numbers []int, target int) []int {
	var left int = 0
	var right int = (len(numbers) - 1)
	for (left < right) {
		var sum int = (numbers[left] + numbers[right])
		if (sum == target) {
			return []int{(left + 1), (right + 1)}
		} else 		if (sum < target) {
			left = (left + 1)
		} else {
			right = (right - 1)
		}
	}
	return _cast[[]int]([]any{})
}

func example_1() {
	expect(_equal(twoSum([]int{2, 7, 11, 15}, 9), []int{1, 2}))
}

func example_2() {
	expect(_equal(twoSum([]int{2, 3, 4}, 6), []int{1, 3}))
}

func example_3() {
	expect(_equal(twoSum([]int{-1, 0}, -1), []int{1, 2}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

