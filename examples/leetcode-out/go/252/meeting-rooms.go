package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canAttendMeetings(intervals [][]int) bool {
	var sorted [][]int = func() [][]int {
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
	_res := [][]int{}
	for _, iv := range items {
		_res = append(_res, iv)
	}
	return _res
}()
	var prevEnd int = -2147483648
	for _, iv := range sorted {
		var start int = iv[0]
		var end int = iv[1]
		if (start < prevEnd) {
			return false
		}
		prevEnd = end
	}
	return true
}

func example_1() {
	expect((canAttendMeetings([][]int{[]int{0, 30}, []int{5, 10}, []int{15, 20}}) == false))
}

func example_2() {
	expect((canAttendMeetings([][]int{[]int{7, 10}, []int{2, 4}}) == true))
}

func empty() {
	expect((canAttendMeetings([][]int{}) == true))
}

func single_interval() {
	expect((canAttendMeetings([][]int{[]int{1, 2}}) == true))
}

func zero_length() {
	expect((canAttendMeetings([][]int{[]int{1, 1}, []int{2, 2}}) == true))
}

func overlap_at_end() {
	expect((canAttendMeetings([][]int{[]int{1, 4}, []int{4, 5}}) == true))
}

func main() {
	example_1()
	example_2()
	empty()
	single_interval()
	zero_length()
	overlap_at_end()
}

