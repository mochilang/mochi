package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maximumGap(nums []int) int {
	var n int = len(nums)
	if (n < 2) {
		return 0
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
	var maxGap int = 0
	for i := 1; i < n; i++ {
		var gap int = (sorted[i] - sorted[(i - 1)])
		if (gap > maxGap) {
			maxGap = gap
		}
	}
	return maxGap
}

func example_1() {
	expect((maximumGap([]int{3, 6, 9, 1}) == 3))
}

func example_2() {
	expect((maximumGap([]int{10}) == 0))
}

func sorted_input() {
	expect((maximumGap([]int{1, 2, 3, 4}) == 1))
}

func duplicates() {
	expect((maximumGap([]int{1, 1, 1, 1}) == 0))
}

func main() {
	example_1()
	example_2()
	sorted_input()
	duplicates()
}

