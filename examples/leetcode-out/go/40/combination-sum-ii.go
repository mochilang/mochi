package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func combinationSum2(candidates []int, target int) [][]int {
	var arr []int = func() []int {
	items := []int{}
	for _, c := range candidates {
		items = append(items, c)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		c := it
		pairs[idx] = pair{item: it, key: c}
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
	_res := []int{}
	for _, c := range items {
		_res = append(_res, c)
	}
	return _res
}()
	var n int = len(arr)
	var result [][]int = [][]int{}
	var backtrack func(int, int, []int)
	backtrack = func(remain int, start int, path []int) {
		if (remain == 0) {
			result = append(append([][]int{}, result...), [][]int{path}...)
		} else {
			var i int = start
			for (i < n) {
				var current int = arr[i]
				if (current > remain) {
					break
				}
				if ((i > start) && (arr[i] == arr[(i - 1)])) {
					i = (i + 1)
					continue
				}
				backtrack((remain - current), (i + 1), append(append([]int{}, path...), []int{current}...))
				i = (i + 1)
			}
		}
}
	backtrack(target, 0, []int{})
	return result
}

func example_1() {
	expect(_equal(combinationSum2([]int{10, 1, 2, 7, 6, 1, 5}, 8), [][]int{[]int{1, 1, 6}, []int{1, 2, 5}, []int{1, 7}, []int{2, 6}}))
}

func example_2() {
	expect(_equal(combinationSum2([]int{2, 5, 2, 1, 2}, 5), [][]int{[]int{1, 2, 2}, []int{5}}))
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

