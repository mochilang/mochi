package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func kthSmallest(matrix [][]int, k int) int {
	var flat []int = []int{}
	for _, row := range matrix {
		flat = append(append([]int{}, flat...), row...)
	}
	var nums []int = func() []int {
	items := []int{}
	for _, n := range flat {
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
	return nums[(k - 1)]
}

func example() {
	expect((kthSmallest(example, 8) == 13))
}

func single_value() {
	expect((kthSmallest([][]int{[]int{7}}, 1) == 7))
}

var example [][]int = [][]int{[]int{1, 5, 9}, []int{10, 11, 13}, []int{12, 13, 15}}
func main() {
	example()
	single_value()
}

