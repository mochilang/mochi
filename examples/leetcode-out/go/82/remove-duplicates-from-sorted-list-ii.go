package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func deleteDuplicates(nums []int) []int {
	var n int = len(nums)
	var result []int = []int{}
	var i int = 0
	for (i < n) {
		var value int = nums[i]
		var j int = (i + 1)
		for (j < n) {
			if (nums[j] == value) {
				j = (j + 1)
			} else {
				break
			}
		}
		if (j == (i + 1)) {
			result = append(append([]int{}, result...), []int{value}...)
		}
		i = j
	}
	return result
}

func example_1() {
	expect(_equal(deleteDuplicates([]int{1, 2, 3, 3, 4, 4, 5}), []int{1, 2, 5}))
}

func example_2() {
	expect(_equal(deleteDuplicates([]int{1, 1, 1, 2, 3}), []int{2, 3}))
}

func empty() {
	expect(_equal(deleteDuplicates([]int{}), []any{}))
}

func no_duplicates() {
	expect(_equal(deleteDuplicates([]int{1, 2, 3}), []int{1, 2, 3}))
}

func main() {
	example_1()
	example_2()
	empty()
	no_duplicates()
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

