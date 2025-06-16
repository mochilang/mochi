package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func sortColors(nums []int) []int {
	var low int = 0
	var mid int = 0
	var high int = (len(nums) - 1)
	for (mid <= high) {
		if (nums[mid] == 0) {
			var temp int = nums[low]
			nums[low] = nums[mid]
			nums[mid] = temp
			low = (low + 1)
			mid = (mid + 1)
		} else 		if (nums[mid] == 1) {
			mid = (mid + 1)
		} else {
			var temp int = nums[mid]
			nums[mid] = nums[high]
			nums[high] = temp
			high = (high - 1)
		}
	}
	return nums
}

func example_1() {
	expect(_equal(sortColors([]int{2, 0, 2, 1, 1, 0}), []int{0, 0, 1, 1, 2, 2}))
}

func example_2() {
	expect(_equal(sortColors([]int{2, 0, 1}), []int{0, 1, 2}))
}

func single_zero() {
	expect(_equal(sortColors([]int{0}), []int{0}))
}

func single_one() {
	expect(_equal(sortColors([]int{1}), []int{1}))
}

func single_two() {
	expect(_equal(sortColors([]int{2}), []int{2}))
}

func main() {
	example_1()
	example_2()
	single_zero()
	single_one()
	single_two()
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

