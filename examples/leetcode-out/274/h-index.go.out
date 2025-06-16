package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func hIndex(citations []int) int {
	var sorted []int = func() []int {
	items := []int{}
	for _, c := range citations {
		items = append(items, c)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		c := it
		pairs[idx] = pair{item: it, key: -c}
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
	var h int = 0
	var i int = 0
	for (i < len(sorted)) {
		if (sorted[i] >= (i + 1)) {
			h = (i + 1)
		}
		i = (i + 1)
	}
	return h
}

func example_1() {
	expect((hIndex([]int{3, 0, 6, 1, 5}) == 3))
}

func example_2() {
	expect((hIndex([]int{1, 3, 1}) == 1))
}

func all_zeros() {
	expect((hIndex([]int{0, 0, 0}) == 0))
}

func all_high() {
	expect((hIndex([]int{10, 8, 5, 4, 3}) == 4))
}

func empty() {
	expect((hIndex([]int{}) == 0))
}

func main() {
	example_1()
	example_2()
	all_zeros()
	all_high()
	empty()
}

