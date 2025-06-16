package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func rotate(nums []int, k int) []int {
	var n int = len(nums)
	if (n == 0) {
		return nums
	}
	var r int = (k % n)
	if (r == 0) {
		return nums
	}
	return append(append([]int{}, nums[(n - r):len(nums)]...), nums[0:(n - r)]...)
}

func example_1() {
	expect(_equal(rotate([]int{1, 2, 3, 4, 5, 6, 7}, 3), []int{5, 6, 7, 1, 2, 3, 4}))
}

func example_2() {
	expect(_equal(rotate([]int{-1, -100, 3, 99}, 2), []int{3, 99, -1, -100}))
}

func k_greater_than_length() {
	expect(_equal(rotate([]int{1, 2}, 5), []int{2, 1}))
}

func main() {
	example_1()
	example_2()
	k_greater_than_length()
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

