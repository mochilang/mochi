package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func threeSumSmaller(nums []int, target int) int {
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
	var n int = len(sorted)
	var count int = 0
	var i int = 0
	for (i < (n - 2)) {
		var left int = (i + 1)
		var right int = (n - 1)
		for (left < right) {
			var sum int = ((sorted[i] + sorted[left]) + sorted[right])
			if (sum < target) {
				count = (count + ((right - left)))
				left = (left + 1)
			} else {
				right = (right - 1)
			}
		}
		i = (i + 1)
	}
	return count
}

func example_1() {
	expect((threeSumSmaller([]int{-2, 0, 1, 3}, 2) == 2))
}

func example_2() {
	expect((threeSumSmaller([]int{}, 0) == 0))
}

func example_3() {
	expect((threeSumSmaller([]int{0}, 0) == 0))
}

func all_negatives() {
	expect((threeSumSmaller([]int{-5, -4, -3, -2}, -1) == 4))
}

func mixed_numbers() {
	expect((threeSumSmaller([]int{-2, 0, 1, 3}, 1) == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	all_negatives()
	mixed_numbers()
}

