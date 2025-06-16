package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func parseInt(s string) int {
	var result int = 0
	var i int = 0
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	for (i < len(s)) {
		result = ((result * 10) + digits[_indexString(s, i)])
		i = (i + 1)
	}
	return result
}

func diffWaysToCompute(expr string) []int {
	var results []int = []int{}
	var i int = 0
	for (i < len(expr)) {
		var ch string = _indexString(expr, i)
		if (((ch == "+") || (ch == "-")) || (ch == "*")) {
			var leftPart string = string([]rune(expr)[0:i])
			var rightPart string = string([]rune(expr)[(i + 1):len(expr)])
			var leftVals []int = diffWaysToCompute(leftPart)
			var rightVals []int = diffWaysToCompute(rightPart)
			for _, a := range leftVals {
				for _, b := range rightVals {
					var val int = 0
					if (ch == "+") {
						val = (a + b)
					} else 					if (ch == "-") {
						val = (a - b)
					} else {
						val = (a * b)
					}
					results = append(append([]int{}, results...), []int{val}...)
				}
			}
		}
		i = (i + 1)
	}
	if (len(results) == 0) {
		results = []int{parseInt(expr)}
	}
	return results
}

func example_1() {
	var res []int = diffWaysToCompute("2-1-1")
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range res {
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
	_ = sorted
	var expected []int = []int{0, 2}
	_ = expected
	expect(_equal(sorted, expected))
}

func example_2() {
	var res []int = diffWaysToCompute("2*3-4*5")
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range res {
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
	_ = sorted
	var expected []int = []int{-34, -14, -10, -10, 10}
	var expSorted []int = func() []int {
	items := []int{}
	for _, x := range expected {
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
	_ = expSorted
	expect(_equal(sorted, expSorted))
}

func single_number() {
	expect(_equal(diffWaysToCompute("3"), []int{3}))
}

func main() {
	example_1()
	example_2()
	single_number()
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

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

