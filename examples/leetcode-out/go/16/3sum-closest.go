package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func threeSumClosest(nums []int, target int) int {
	var sorted []int = func() []int {
	items := []int{}
	for _, n := range nums {
		items = append(items, n)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		n := it
		pairs[idx] = pair{item: it, key: n}
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
	for _, n := range items {
		_res = append(_res, n)
	}
	return _res
}()
	var n int = len(sorted)
	var best int = ((sorted[0] + sorted[1]) + sorted[2])
	for i := 0; i < n; i++ {
		var left int = (i + 1)
		var right int = (n - 1)
		for (left < right) {
			var sum int = ((sorted[i] + sorted[left]) + sorted[right])
			if (sum == target) {
				return target
			}
			var diff int = 0
			if (sum > target) {
				diff = (sum - target)
			} else {
				diff = (target - sum)
			}
			var bestDiff int = 0
			if (best > target) {
				bestDiff = (best - target)
			} else {
				bestDiff = (target - best)
			}
			if (diff < bestDiff) {
				best = sum
			}
			if (sum < target) {
				left = (left + 1)
			} else {
				right = (right - 1)
			}
		}
	}
	return best
}

func example_1() {
	expect((threeSumClosest([]int{-1, 2, 1, -4}, 1) == 2))
}

func example_2() {
	expect((threeSumClosest([]int{0, 0, 0}, 1) == 0))
}

func additional() {
	expect((threeSumClosest([]int{1, 1, 1, 0}, -100) == 2))
}

func main() {
	example_1()
	example_2()
	additional()
}

