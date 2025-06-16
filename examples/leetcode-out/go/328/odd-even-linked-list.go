package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func oddEvenList(nums []int) []int {
	var odd []int = []int{}
	var even []int = []int{}
	var i int = 0
	var n int = len(nums)
	for (i < n) {
		if (((i % 2)) == 0) {
			odd = append(append([]int{}, odd...), []int{nums[i]}...)
		} else {
			even = append(append([]int{}, even...), []int{nums[i]}...)
		}
		i = (i + 1)
	}
	return append(append([]int{}, odd...), even...)
}

func example_1() {
	expect(_equal(oddEvenList([]int{1, 2, 3, 4, 5}), []int{1, 3, 5, 2, 4}))
}

func example_2() {
	expect(_equal(oddEvenList([]int{2, 1, 3, 5, 6, 4, 7}), []int{2, 3, 6, 7, 1, 5, 4}))
}

func single_element() {
	expect(_equal(oddEvenList([]int{1}), []int{1}))
}

func empty_list() {
	expect(_equal(oddEvenList([]int{}), _cast[[]int]([]any{})))
}

func main() {
	example_1()
	example_2()
	single_element()
	empty_list()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
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

