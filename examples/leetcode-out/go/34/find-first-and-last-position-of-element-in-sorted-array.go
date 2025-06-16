package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func searchRange(nums []int, target int) []int {
	var n int = len(nums)
	var left int = 0
	var right int = (n - 1)
	var start int = -1
	var end int = -1
	left = 0
	right = (n - 1)
	for (left <= right) {
		var mid int = (((left + right)) / 2)
		if (nums[mid] == target) {
			start = mid
			right = (mid - 1)
		} else 		if (nums[mid] < target) {
			left = (mid + 1)
		} else {
			right = (mid - 1)
		}
	}
	if (start == (-1)) {
		return []int{-1, -1}
	}
	left = start
	right = (n - 1)
	for (left <= right) {
		var mid int = (((left + right)) / 2)
		if (nums[mid] == target) {
			end = mid
			left = (mid + 1)
		} else 		if (nums[mid] < target) {
			left = (mid + 1)
		} else {
			right = (mid - 1)
		}
	}
	return []int{start, end}
}

func example_1() {
	expect(_equal(searchRange([]int{5, 7, 7, 8, 8, 10}, 8), []int{3, 4}))
}

func example_2() {
	expect(_equal(searchRange([]int{5, 7, 7, 8, 8, 10}, 6), []int{-1, -1}))
}

func example_3() {
	expect(_equal(searchRange([]int{}, 0), []int{-1, -1}))
}

func single_element_match() {
	expect(_equal(searchRange([]int{1}, 1), []int{0, 0}))
}

func single_element_missing() {
	expect(_equal(searchRange([]int{1}, 0), []int{-1, -1}))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_element_match()
	single_element_missing()
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

