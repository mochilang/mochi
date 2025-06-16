package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func intersection(nums1 []int, nums2 []int) []int {
	var set1 map[int]bool = map[int]bool{}
	for _, n := range nums1 {
		set1[n] = true
	}
	var seen map[int]bool = map[int]bool{}
	var result []int = []int{}
	for _, n := range nums2 {
		_tmp0 := n
		_tmp1 := set1
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			_tmp3 := n
			_tmp4 := seen
			_, _tmp5 := _tmp4[_tmp3]
			if !(_tmp5) {
				result = append(append([]int{}, result...), []int{n}...)
				seen[n] = true
			}
		}
	}
	return result
}

func example_1() {
	var out []int = intersection([]int{1, 2, 2, 1}, []int{2, 2})
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range out {
		items = append(items, x)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x}
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
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	_ = sorted
	expect(_equal(sorted, []int{2}))
}

func example_2() {
	var out []int = intersection([]int{4, 9, 5}, []int{9, 4, 9, 8, 4})
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range out {
		items = append(items, x)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x}
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
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	_ = sorted
	expect(_equal(sorted, []int{4, 9}))
}

func empty_first() {
	expect(_equal(intersection([]int{}, []int{1, 2}), []any{}))
}

func empty_second() {
	expect(_equal(intersection([]int{1, 2, 3}, []int{}), []any{}))
}

func main() {
	example_1()
	example_2()
	empty_first()
	empty_second()
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

