package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getRow(rowIndex int) []int {
	var row []int = []int{1}
	var i int = 0
	for (i < rowIndex) {
		var next []int = []int{1}
		var j int = 1
		for (j < len(row)) {
			next = append(append([]int{}, next...), []int{(row[(j - 1)] + row[j])}...)
			j = (j + 1)
		}
		next = append(append([]int{}, next...), []int{1}...)
		row = next
		i = (i + 1)
	}
	return row
}

func example_1() {
	expect(_equal(getRow(3), []int{1, 3, 3, 1}))
}

func example_2() {
	expect(_equal(getRow(0), []int{1}))
}

func example_3() {
	expect(_equal(getRow(1), []int{1, 1}))
}

func row_2() {
	expect(_equal(getRow(2), []int{1, 2, 1}))
}

func row_4() {
	expect(_equal(getRow(4), []int{1, 4, 6, 4, 1}))
}

func main() {
	example_1()
	example_2()
	example_3()
	row_2()
	row_4()
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

