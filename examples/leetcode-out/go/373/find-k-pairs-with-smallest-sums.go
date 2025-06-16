package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func kSmallestPairs(nums1 []int, nums2 []int, k int) [][]int {
	var pairs [][]int = [][]int{}
	for _, a := range nums1 {
		for _, b := range nums2 {
			pairs = append(append([][]int{}, pairs...), [][]int{[]int{a, b}}...)
		}
	}
	var sorted [][]int = func() [][]int {
	items := [][]int{}
	for _, p := range pairs {
		items = append(items, p)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		p := it
		pairs[idx] = pair{item: it, key: (p[0] + p[1])}
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
	for _, p := range items {
		_res = append(_res, p)
	}
	return _res
}()
	if (k < len(sorted)) {
		return sorted[0:k]
	}
	return sorted
}

func example_1() {
	expect(_equal(kSmallestPairs([]int{1, 7, 11}, []int{2, 4, 6}, 3), [][]int{[]int{1, 2}, []int{1, 4}, []int{1, 6}}))
}

func example_2() {
	expect(_equal(kSmallestPairs([]int{1, 1, 2}, []int{1, 2, 3}, 2), [][]int{[]int{1, 1}, []int{1, 1}}))
}

func example_3() {
	expect(_equal(kSmallestPairs([]int{1, 2}, []int{3}, 3), [][]int{[]int{1, 3}, []int{2, 3}}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

