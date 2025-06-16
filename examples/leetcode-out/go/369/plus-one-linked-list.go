package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func plusOne(head []int) []int {
	var carry int = 1
	var i int = (len(head) - 1)
	var result []int = head
	for ((i >= 0) && (carry > 0)) {
		var sum int = (result[i] + carry)
		result[i] = (sum % 10)
		carry = (sum / 10)
		i = (i - 1)
	}
	if (carry > 0) {
		return append(append([]int{}, []int{carry}...), result...)
	}
	return result
}

func example_1() {
	expect(_equal(plusOne([]int{1, 2, 3}), []int{1, 2, 4}))
}

func example_2() {
	expect(_equal(plusOne([]int{0}), []int{1}))
}

func carry_through() {
	expect(_equal(plusOne([]int{9, 9, 9}), []int{1, 0, 0, 0}))
}

func middle_carry() {
	expect(_equal(plusOne([]int{1, 2, 9}), []int{1, 3, 0}))
}

func main() {
	example_1()
	example_2()
	carry_through()
	middle_carry()
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

