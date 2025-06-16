package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func groupAnagrams(strs []string) [][]string {
	var index map[string]int = map[string]int{}
	var groups [][]string = [][]string{}
	for _, word := range strs {
		var letters []string = []string{}
		var i int = 0
		for (i < len(word)) {
			letters = append(append([]string{}, letters...), []string{_indexString(word, i)}...)
			i = (i + 1)
		}
		var chars []string = func() []string {
	items := []string{}
	for _, ch := range letters {
		items = append(items, ch)
	}
	type pair struct { item string; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		ch := it
		pairs[idx] = pair{item: it, key: ch}
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
	_res := []string{}
	for _, ch := range items {
		_res = append(_res, ch)
	}
	return _res
}()
		var key string = ""
		for _, ch := range chars {
			key = key + ch
		}
		_tmp0 := key
		_tmp1 := index
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			var idx int = index[key]
			groups[idx] = append(append([]string{}, groups[idx]...), []string{word}...)
		} else {
			groups = append(append([][]string{}, groups...), [][]string{[]string{word}}...)
			index[key] = (len(groups) - 1)
		}
	}
	return groups
}

func example_1() {
	var res [][]string = groupAnagrams([]string{"eat", "tea", "tan", "ate", "nat", "bat"})
	var normalize func([]string) []string = func(g []string) []string {
		return func() []string {
		items := []string{}
		for _, x := range g {
			items = append(items, x)
		}
		type pair struct { item string; key any }
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
		_res := []string{}
		for _, x := range items {
			_res = append(_res, x)
		}
		return _res
	}()
}
	var sorted [][]string = func() [][]string {
	_res := [][]string{}
	for _, g := range res {
		_res = append(_res, normalize(g))
	}
	return _res
}()
	var final [][]string = func() [][]string {
	items := [][]string{}
	for _, g := range sorted {
		items = append(items, g)
	}
	type pair struct { item []string; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		g := it
		pairs[idx] = pair{item: it, key: g[0]}
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
	_res := [][]string{}
	for _, g := range items {
		_res = append(_res, g)
	}
	return _res
}()
	_ = final
	var expected [][]string = [][]string{[]string{"ate", "eat", "tea"}, []string{"bat"}, []string{"nat", "tan"}}
	var expSorted [][]string = func() [][]string {
	_res := [][]string{}
	for _, g := range expected {
		_res = append(_res, normalize(g))
	}
	return _res
}()
	var expFinal [][]string = func() [][]string {
	items := [][]string{}
	for _, g := range expSorted {
		items = append(items, g)
	}
	type pair struct { item []string; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		g := it
		pairs[idx] = pair{item: it, key: g[0]}
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
	_res := [][]string{}
	for _, g := range items {
		_res = append(_res, g)
	}
	return _res
}()
	_ = expFinal
	expect(_equal(final, expFinal))
}

func example_2() {
	expect(_equal(groupAnagrams([]string{""}), [][]string{[]string{""}}))
}

func example_3() {
	expect(_equal(groupAnagrams([]string{"a"}), [][]string{[]string{"a"}}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

