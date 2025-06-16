package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func reorderList(nums []int) []int {
	var n int = len(nums)
	var result []int = []int{}
	var i int = 0
	var j int = (n - 1)
	for (i <= j) {
		if (i == j) {
			result = append(append([]int{}, result...), []int{nums[i]}...)
		} else {
			result = append(append([]int{}, result...), []int{nums[i], nums[j]}...)
		}
		i = (i + 1)
		j = (j - 1)
	}
	return result
}

func example_1() {
	expect(_equal(reorderList([]int{1, 2, 3, 4}), []int{1, 4, 2, 3}))
}

func example_2() {
	expect(_equal(reorderList([]int{1, 2, 3, 4, 5}), []int{1, 5, 2, 4, 3}))
}

func single_element() {
	expect(_equal(reorderList([]int{1}), []int{1}))
}

func main() {
	example_1()
	example_2()
	single_element()
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

