package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func subsets(nums []int) [][]int {
	var result [][]int = [][]int{[]int{}}
	for _, num := range nums {
		var newSets [][]int = [][]int{}
		var i int = 0
		for (i < len(result)) {
			var subset []int = result[i]
			newSets = append(append([][]int{}, newSets...), [][]int{append(append([]int{}, subset...), []int{num}...)}...)
			i = (i + 1)
		}
		var j int = 0
		for (j < len(newSets)) {
			result = append(append([][]int{}, result...), [][]int{newSets[j]}...)
			j = (j + 1)
		}
	}
	return result
}

func example_1() {
	expect(_equal(subsets([]int{1, 2, 3}), []any{[]any{}, []int{1}, []int{2}, []int{1, 2}, []int{3}, []int{1, 3}, []int{2, 3}, []int{1, 2, 3}}))
}

func example_2() {
	expect(_equal(subsets([]int{0}), []any{[]any{}, []int{0}}))
}

func main() {
	example_1()
	example_2()
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

