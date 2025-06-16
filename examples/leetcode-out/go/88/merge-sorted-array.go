package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func merge(nums1 []int, m int, nums2 []int, n int) []int {
	var i int = (m - 1)
	var j int = (n - 1)
	var k int = ((m + n) - 1)
	for (j >= 0) {
		if ((i >= 0) && (nums1[i] > nums2[j])) {
			nums1[k] = nums1[i]
			i = (i - 1)
		} else {
			nums1[k] = nums2[j]
			j = (j - 1)
		}
		k = (k - 1)
	}
	return nums1
}

func example_1() {
	expect(_equal(merge([]int{1, 2, 3, 0, 0, 0}, 3, []int{2, 5, 6}, 3), []int{1, 2, 2, 3, 5, 6}))
}

func example_2() {
	expect(_equal(merge([]int{1}, 1, []int{}, 0), []int{1}))
}

func example_3() {
	expect(_equal(merge([]int{0}, 0, []int{1}, 1), []int{1}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

