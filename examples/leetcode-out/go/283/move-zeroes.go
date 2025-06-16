package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func moveZeroes(nums []int) []int {
	var result []int = nums
	var n int = len(nums)
	var insert int = 0
	var i int = 0
	for (i < n) {
		if (nums[i] != 0) {
			result[insert] = nums[i]
			insert = (insert + 1)
		}
		i = (i + 1)
	}
	for (insert < n) {
		result[insert] = 0
		insert = (insert + 1)
	}
	return result
}

func example_1() {
	expect(_equal(moveZeroes([]int{0, 1, 0, 3, 12}), []int{1, 3, 12, 0, 0}))
}

func example_2() {
	expect(_equal(moveZeroes([]int{0}), []int{0}))
}

func all_zeros() {
	expect(_equal(moveZeroes([]int{0, 0, 0}), []int{0, 0, 0}))
}

func no_zeros() {
	expect(_equal(moveZeroes([]int{1, 2, 3}), []int{1, 2, 3}))
}

func mixed() {
	expect(_equal(moveZeroes([]int{4, 0, 5, 0, 6}), []int{4, 5, 6, 0, 0}))
}

func main() {
	example_1()
	example_2()
	all_zeros()
	no_zeros()
	mixed()
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

