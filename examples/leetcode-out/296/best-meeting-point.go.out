package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func abs(x int) int {
	if (x < 0) {
		return -x
	} else {
		return x
	}
}

func minTotalDistance(grid [][]int) int {
	var rows []int = []int{}
	var cols []int = []int{}
	var i int = 0
	for (i < len(grid)) {
		var j int = 0
		var row []int = grid[i]
		for (j < len(row)) {
			if (row[j] == 1) {
				rows = append(append([]int{}, rows...), []int{i}...)
				cols = append(append([]int{}, cols...), []int{j}...)
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	var sortedRows []int = func() []int {
	items := []int{}
	for _, r := range rows {
		items = append(items, r)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		r := it
		pairs[idx] = pair{item: it, key: r}
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
	for _, r := range items {
		_res = append(_res, r)
	}
	return _res
}()
	var sortedCols []int = func() []int {
	items := []int{}
	for _, c := range cols {
		items = append(items, c)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		c := it
		pairs[idx] = pair{item: it, key: c}
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
	var mid int = (len(sortedRows) / 2)
	var rowMedian int = sortedRows[mid]
	var colMedian int = sortedCols[mid]
	var dist int = 0
	var k int = 0
	for (k < len(sortedRows)) {
		dist = (dist + abs((sortedRows[k] - rowMedian)))
		k = (k + 1)
	}
	k = 0
	for (k < len(sortedCols)) {
		dist = (dist + abs((sortedCols[k] - colMedian)))
		k = (k + 1)
	}
	return dist
}

func example_1() {
	expect((minTotalDistance([][]int{[]int{1, 0, 0, 0, 1}, []int{0, 0, 0, 0, 0}, []int{0, 0, 1, 0, 0}}) == 6))
}

func example_2() {
	expect((minTotalDistance([][]int{[]int{1, 1}}) == 1))
}

func single_column() {
	expect((minTotalDistance([][]int{[]int{1}, []int{1}}) == 1))
}

func single_cell() {
	expect((minTotalDistance([][]int{[]int{1}}) == 0))
}

func main() {
	example_1()
	example_2()
	single_column()
	single_cell()
}

