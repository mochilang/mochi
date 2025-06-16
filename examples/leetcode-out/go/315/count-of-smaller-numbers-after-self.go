package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countSmaller(nums []int) []int {
	var n int = len(nums)
	var result []int = []int{}
	var fill int = 0
	for (fill < n) {
		result = append(append([]int{}, result...), []int{0}...)
		fill = (fill + 1)
	}
	if (n == 0) {
		return result
	}
	var i int = (n - 1)
	for (i >= 0) {
		var count int = 0
		var j int = (i + 1)
		for (j < n) {
			if (nums[j] < nums[i]) {
				count = (count + 1)
			}
			j = (j + 1)
		}
		result[i] = count
		i = (i - 1)
	}
	return result
}

func example_1() {
	expect(_equal(countSmaller([]int{5, 2, 6, 1}), []int{2, 1, 1, 0}))
}

func example_2() {
	expect(_equal(countSmaller([]int{-1}), []int{0}))
}

func example_3() {
	expect(_equal(countSmaller([]int{-1, -1}), []int{0, 0}))
}

func empty() {
	expect(_equal(countSmaller([]int{}), []any{}))
}

func single_value() {
	expect(_equal(countSmaller([]int{7}), []int{0}))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty()
	single_value()
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

