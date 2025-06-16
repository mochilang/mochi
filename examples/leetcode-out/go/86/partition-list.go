package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func partition(head []int, x int) []int {
	var less []int = []int{}
	var greater []int = []int{}
	var idx int = 0
	for (idx < len(head)) {
		var val int = head[idx]
		if (val < x) {
			less = append(append([]int{}, less...), []int{val}...)
		} else {
			greater = append(append([]int{}, greater...), []int{val}...)
		}
		idx = (idx + 1)
	}
	var j int = 0
	for (j < len(greater)) {
		var val int = greater[j]
		less = append(append([]int{}, less...), []int{val}...)
		j = (j + 1)
	}
	return less
}

func example_1() {
	expect(_equal(partition([]int{1, 4, 3, 2, 5, 2}, 3), []int{1, 2, 2, 4, 3, 5}))
}

func example_2() {
	expect(_equal(partition([]int{2, 1}, 2), []int{1, 2}))
}

func all_less() {
	expect(_equal(partition([]int{1, 1, 1}, 5), []int{1, 1, 1}))
}

func main() {
	example_1()
	example_2()
	all_less()
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

