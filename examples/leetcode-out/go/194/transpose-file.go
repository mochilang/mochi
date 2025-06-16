package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func transpose(rows [][]int) [][]int {
	if (len(rows) == 0) {
		return _cast[[][]int]([]any{})
	}
	var row_count int = len(rows)
	var col_count int = len(rows[0])
	var result [][]int = [][]int{}
	var c int = 0
	for (c < col_count) {
		var new_row []int = []int{}
		var r int = 0
		for (r < row_count) {
			new_row = append(append([]int{}, new_row...), []int{rows[r][c]}...)
			r = (r + 1)
		}
		result = append(append([][]int{}, result...), [][]int{new_row}...)
		c = (c + 1)
	}
	return result
}

func square_matrix() {
	expect(_equal(transpose([][]int{[]int{1, 2, 3}, []int{4, 5, 6}, []int{7, 8, 9}}), [][]int{[]int{1, 4, 7}, []int{2, 5, 8}, []int{3, 6, 9}}))
}

func rectangular_matrix() {
	expect(_equal(transpose([][]int{[]int{1, 2}, []int{3, 4}, []int{5, 6}}), [][]int{[]int{1, 3, 5}, []int{2, 4, 6}}))
}

func main() {
	square_matrix()
	rectangular_matrix()
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

