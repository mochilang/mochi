package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func minMeetingRooms(intervals [][]int) int {
	if (len(intervals) == 0) {
		return 0
	}
	var starts []int = func() []int {
	items := [][]int{}
	for _, iv := range intervals {
		items = append(items, iv)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		iv := it
		pairs[idx] = pair{item: it, key: iv[0]}
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
	for _, iv := range items {
		_res = append(_res, iv[0])
	}
	return _res
}()
	var ends []int = func() []int {
	items := [][]int{}
	for _, iv := range intervals {
		items = append(items, iv)
	}
	type pair struct { item []int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		iv := it
		pairs[idx] = pair{item: it, key: iv[1]}
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
	for _, iv := range items {
		_res = append(_res, iv[1])
	}
	return _res
}()
	var s int = 0
	var e int = 0
	var rooms int = 0
	var maxRooms int = 0
	var n int = len(intervals)
	for (s < n) {
		if (starts[s] < ends[e]) {
			rooms = (rooms + 1)
			s = (s + 1)
			if (rooms > maxRooms) {
				maxRooms = rooms
			}
		} else {
			rooms = (rooms - 1)
			e = (e + 1)
		}
	}
	return maxRooms
}

func example_1() {
	expect((minMeetingRooms([][]int{[]int{0, 30}, []int{5, 10}, []int{15, 20}}) == 2))
}

func example_2() {
	expect((minMeetingRooms([][]int{[]int{7, 10}, []int{2, 4}}) == 1))
}

func no_meetings() {
	expect((minMeetingRooms([][]int{}) == 0))
}

func same_start_end() {
	expect((minMeetingRooms([][]int{[]int{0, 10}, []int{10, 20}}) == 1))
}

func main() {
	example_1()
	example_2()
	no_meetings()
	same_start_end()
}

