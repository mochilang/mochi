package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func merge(intervals [][]int) [][]int {
	if (len(intervals) == 0) {
		return _cast[[][]int]([]any{})
	}
	var sorted [][]int = func() [][]int {
	items := [][]int{}
	for _, x := range intervals {
		items = append(items, x)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x[0]}
	}
	sort.Slice(pairs, func(i, j int) bool {
		a, b := pairs[i].key, pairs[j].key
		switch av := a.(type) {
		case int:
			switch bv := b.(type) {
			case int:
				return av < bv
			case float64:
				return float64(av) < bv
			}
		case float64:
			switch bv := b.(type) {
			case int:
				return av < float64(bv)
			case float64:
				return av < bv
			}
		case string:
			bs, _ := b.(string)
			return av < bs
		}
		return fmt.Sprint(a) < fmt.Sprint(b)
	})
	for idx, p := range pairs {
		items[idx] = p.item
	}
	_res := [][]int{}
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	var result [][]int = [][]int{}
	for _, inter := range sorted {
		if (len(result) == 0) {
			result = append(append([][]int{}, result...), [][]int{inter}...)
		} else 		if (result[(len(result) - 1)][1] < inter[0]) {
			result = append(append([][]int{}, result...), [][]int{inter}...)
		} else 		if (inter[1] > result[(len(result) - 1)][1]) {
			result[(len(result) - 1)][1] = inter[1]
		}
	}
	return result
}

func example_1() {
	expect(_equal(merge([][]int{[]int{1, 3}, []int{2, 6}, []int{8, 10}, []int{15, 18}}), [][]int{[]int{1, 6}, []int{8, 10}, []int{15, 18}}))
}

func example_2() {
	expect(_equal(merge([][]int{[]int{1, 4}, []int{4, 5}}), [][]int{[]int{1, 5}}))
}

func single_interval() {
	expect(_equal(merge([][]int{[]int{1, 4}}), [][]int{[]int{1, 4}}))
}

func empty_list() {
	expect(_equal(merge([][]int{}), []any{}))
}

func main() {
	example_1()
	example_2()
	single_interval()
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

