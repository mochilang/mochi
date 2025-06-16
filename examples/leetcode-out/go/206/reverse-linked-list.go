package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func reverseList(nums []int) []int {
	var result []int = []int{}
	var i int = (len(nums) - 1)
	for (i >= 0) {
		result = append(append([]int{}, result...), []int{nums[i]}...)
		i = (i - 1)
	}
	return result
}

func example_1() {
	expect(_equal(reverseList([]int{1, 2, 3, 4, 5}), []int{5, 4, 3, 2, 1}))
}

func example_2() {
	expect(_equal(reverseList([]int{1, 2}), []int{2, 1}))
}

func single_element() {
	expect(_equal(reverseList([]int{1}), []int{1}))
}

func empty_list() {
	expect(_equal(reverseList([]int{}), []any{}))
}

func main() {
	example_1()
	example_2()
	single_element()
	empty_list()
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

