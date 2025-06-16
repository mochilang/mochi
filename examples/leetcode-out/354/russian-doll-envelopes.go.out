package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxEnvelopes(envelopes [][]int) int {
	var n int = len(envelopes)
	if (n == 0) {
		return 0
	}
	var byH [][]int = func() [][]int {
	items := [][]int{}
	for _, e := range envelopes {
		items = append(items, e)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		e := it
		pairs[idx] = pair{item: it, key: -e[1]}
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
	for _, e := range items {
		_res = append(_res, e)
	}
	return _res
}()
	var sorted [][]int = func() [][]int {
	items := [][]int{}
	for _, e := range byH {
		items = append(items, e)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		e := it
		pairs[idx] = pair{item: it, key: e[0]}
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
	for _, e := range items {
		_res = append(_res, e)
	}
	return _res
}()
	var tails []int = []int{}
	var fill int = 0
	for (fill < n) {
		tails = append(append([]int{}, tails...), []int{0}...)
		fill = (fill + 1)
	}
	var size int = 0
	var i int = 0
	for (i < n) {
		var height int = sorted[i][1]
		var lo int = 0
		var hi int = size
		for (lo < hi) {
			var mid int = (((lo + hi)) / 2)
			if (tails[mid] < height) {
				lo = (mid + 1)
			} else {
				hi = mid
			}
		}
		tails[lo] = height
		if (lo == size) {
			size = (size + 1)
		}
		i = (i + 1)
	}
	return size
}

func example_1() {
	expect((maxEnvelopes([][]int{[]int{5, 4}, []int{6, 4}, []int{6, 7}, []int{2, 3}}) == 3))
}

func example_2() {
	expect((maxEnvelopes([][]int{[]int{1, 1}, []int{1, 1}, []int{1, 1}}) == 1))
}

func empty() {
	expect((maxEnvelopes([][]int{}) == 0))
}

func increasing() {
	expect((maxEnvelopes([][]int{[]int{1, 1}, []int{2, 2}, []int{3, 3}, []int{4, 4}}) == 4))
}

func main() {
	example_1()
	example_2()
	empty()
	increasing()
}

