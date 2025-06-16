package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func wiggleSort(nums []int) []int {
	var n int = len(nums)
	if (n <= 1) {
		return nums
	}
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range nums {
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
	var left int = ((((n + 1)) / 2) - 1)
	var right int = (n - 1)
	var result []int = []int{}
	for (len(result) < n) {
		if (left >= 0) {
			result = append(append([]int{}, result...), []int{sorted[left]}...)
			left = (left - 1)
		}
		if ((len(result) < n) && (right >= (((n + 1)) / 2))) {
			result = append(append([]int{}, result...), []int{sorted[right]}...)
			right = (right - 1)
		}
	}
	return result
}

func example_1() {
	expect(_equal(wiggleSort([]int{1, 5, 1, 1, 6, 4}), []int{1, 6, 1, 5, 1, 4}))
}

func example_2() {
	expect(_equal(wiggleSort([]int{1, 3, 2, 2, 3, 1}), []int{2, 3, 1, 3, 1, 2}))
}

func single_element() {
	expect(_equal(wiggleSort([]int{1}), []int{1}))
}

func already_wiggle() {
	expect(_equal(wiggleSort([]int{2, 5, 1, 6}), []int{2, 6, 1, 5}))
}

func main() {
	example_1()
	example_2()
	single_element()
	already_wiggle()
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

