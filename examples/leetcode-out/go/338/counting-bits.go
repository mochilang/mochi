package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countBits(n int) []int {
	var result []int = []int{}
	var i int = 0
	for (i <= n) {
		if (i == 0) {
			result = append(append([]int{}, result...), []int{0}...)
		} else {
			var bits int = (result[(i / 2)] + ((i % 2)))
			result = append(append([]int{}, result...), []int{bits}...)
		}
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect(_equal(countBits(2), []int{0, 1, 1}))
}

func example_2() {
	expect(_equal(countBits(5), []int{0, 1, 1, 2, 1, 2}))
}

func zero() {
	expect(_equal(countBits(0), []int{0}))
}

func larger() {
	expect(_equal(countBits(8), []int{0, 1, 1, 2, 1, 2, 2, 3, 1}))
}

func main() {
	example_1()
	example_2()
	zero()
	larger()
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

