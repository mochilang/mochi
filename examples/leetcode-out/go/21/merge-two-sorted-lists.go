package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func mergeTwoLists(l1 []int, l2 []int) []int {
	var i int = 0
	var j int = 0
	var result []any = []any{}
	for ((i < len(l1)) && (j < len(l2))) {
		if (l1[i] <= l2[j]) {
			result = append(append([]any{}, result...), _toAnySlice([]int{l1[i]})...)
			i = (i + 1)
		} else {
			result = append(append([]any{}, result...), _toAnySlice([]int{l2[j]})...)
			j = (j + 1)
		}
	}
	for (i < len(l1)) {
		result = append(append([]any{}, result...), _toAnySlice([]int{l1[i]})...)
		i = (i + 1)
	}
	for (j < len(l2)) {
		result = append(append([]any{}, result...), _toAnySlice([]int{l2[j]})...)
		j = (j + 1)
	}
	return _cast[[]int](result)
}

func example_1() {
	expect(_equal(mergeTwoLists([]int{1, 2, 4}, []int{1, 3, 4}), []int{1, 1, 2, 3, 4, 4}))
}

func example_2() {
	expect(_equal(mergeTwoLists([]int{}, []int{}), []any{}))
}

func example_3() {
	expect(_equal(mergeTwoLists([]int{}, []int{0}), []int{0}))
}

func different_lengths() {
	expect(_equal(mergeTwoLists([]int{1, 5, 7}, []int{2, 3, 4, 6, 8}), []int{1, 2, 3, 4, 5, 6, 7, 8}))
}

func one_list_empty() {
	expect(_equal(mergeTwoLists([]int{1, 2, 3}, []int{}), []int{1, 2, 3}))
}

func main() {
	example_1()
	example_2()
	example_3()
	different_lengths()
	one_list_empty()
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

func _toAnySlice[T any](s []T) []any {
    out := make([]any, len(s))
    for i, v := range s { out[i] = v }
    return out
}

