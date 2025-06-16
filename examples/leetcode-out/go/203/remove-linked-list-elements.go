package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func removeElements(nums []int, val int) []int {
	var result []int = []int{}
	for _, x := range nums {
		if (x != val) {
			result = append(append([]int{}, result...), []int{x}...)
		}
	}
	return result
}

func example_1() {
	expect(_equal(removeElements([]int{1, 2, 6, 3, 4, 5, 6}, 6), []int{1, 2, 3, 4, 5}))
}

func example_2() {
	expect(_equal(removeElements([]int{}, 1), []any{}))
}

func example_3() {
	expect(_equal(removeElements([]int{7, 7, 7, 7}, 7), []any{}))
}

func no_removals() {
	expect(_equal(removeElements([]int{1, 2, 3}, 4), []int{1, 2, 3}))
}

func all_removed() {
	expect(_equal(removeElements([]int{2, 2, 2}, 2), []any{}))
}

func main() {
	example_1()
	example_2()
	example_3()
	no_removals()
	all_removed()
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

