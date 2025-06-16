package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getSkyline(buildings [][]int) [][]int {
	var edges [][]int = [][]int{}
	for _, b := range buildings {
		edges = append(append([][]int{}, edges...), [][]int{[]int{b[0], -b[2]}}...)
		edges = append(append([][]int{}, edges...), [][]int{[]int{b[1], b[2]}}...)
	}
	var byH [][]int = func() [][]int {
	items := [][]int{}
	for _, e := range edges {
		items = append(items, e)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		e := it
		pairs[idx] = pair{item: it, key: e[1]}
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
	var heights []int = []int{0}
	var prev int = 0
	var result [][]int = [][]int{}
	for _, edge := range sorted {
		var x int = edge[0]
		var h int = edge[1]
		if (h < 0) {
			heights = append(append([]int{}, heights...), []int{-h}...)
		} else {
			var idx int = -1
			for i := 0; i < len(heights); i++ {
				if (heights[i] == h) {
					idx = i
					break
				}
			}
			if (idx >= 0) {
				heights = append(append([]int{}, heights[0:idx]...), heights[(idx + 1):len(heights)]...)
			}
		}
		var curr int = 0
		for _, ht := range heights {
			if (ht > curr) {
				curr = ht
			}
		}
		if (curr != prev) {
			result = append(append([][]int{}, result...), [][]int{[]int{x, curr}}...)
			prev = curr
		}
	}
	return result
}

func example_1() {
	expect(_equal(getSkyline([][]int{[]int{2, 9, 10}, []int{3, 7, 15}, []int{5, 12, 12}, []int{15, 20, 10}, []int{19, 24, 8}}), [][]int{[]int{2, 10}, []int{3, 15}, []int{7, 12}, []int{12, 0}, []int{15, 10}, []int{20, 8}, []int{24, 0}}))
}

func example_2() {
	expect(_equal(getSkyline([][]int{[]int{0, 2, 3}, []int{2, 5, 3}}), [][]int{[]int{0, 3}, []int{5, 0}}))
}

func single_building() {
	expect(_equal(getSkyline([][]int{[]int{1, 5, 4}}), [][]int{[]int{1, 4}, []int{5, 0}}))
}

func overlap() {
	expect(_equal(getSkyline([][]int{[]int{1, 3, 3}, []int{1, 4, 2}, []int{3, 4, 1}}), [][]int{[]int{1, 3}, []int{3, 2}, []int{4, 0}}))
}

func main() {
	example_1()
	example_2()
	single_building()
	overlap()
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

