package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func rotateRight(nums []int, k int) []int {
	var n int = len(nums)
	if (n == 0) {
		return nums
	}
	var r int = (k % n)
	if (r == 0) {
		return nums
	}
	var result []int = []int{}
	var i int = 0
	for (i < n) {
		var idx int = ((((n - r) + i)) % n)
		result = append(append([]int{}, result...), []int{nums[idx]}...)
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect(_equal(rotateRight([]int{1, 2, 3, 4, 5}, 2), []int{4, 5, 1, 2, 3}))
}

func example_2() {
	expect(_equal(rotateRight([]int{0, 1, 2}, 4), []int{2, 0, 1}))
}

func k_is_zero() {
	expect(_equal(rotateRight([]int{1, 2, 3}, 0), []int{1, 2, 3}))
}

func empty_list() {
	expect(_equal(rotateRight([]int{}, 5), []any{}))
}

func main() {
	example_1()
	example_2()
	k_is_zero()
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

