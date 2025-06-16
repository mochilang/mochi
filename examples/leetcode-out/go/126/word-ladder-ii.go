package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findLadders(beginWord string, endWord string, wordList []string) [][]string {
	var dict map[string]bool = map[string]bool{}
	for _, w := range wordList {
		dict[w] = true
	}
	_tmp0 := endWord
	_tmp1 := dict
	_, _tmp2 := _tmp1[_tmp0]
	if !(_tmp2) {
		return _cast[[][]string]([]any{})
	}
	var letters string = "abcdefghijklmnopqrstuvwxyz"
	var queue []string = []string{beginWord}
	var visited map[string]int = map[string]int{"beginWord": 0}
	var parents map[string][]string = map[string][]string{}
	var step int = 0
	var found bool = false
	for (len(queue) > 0) {
		if found {
			break
		}
		step = (step + 1)
		var next []string = []string{}
		for _, word := range queue {
			var i int = 0
			for (i < len(word)) {
				var j int = 0
				for (j < 26) {
					var ch string = _indexString(letters, j)
					if (ch != _indexString(word, i)) {
						var candidate string = string([]rune(word)[0:i]) + ch + string([]rune(word)[(i + 1):len(word)])
						_tmp3 := candidate
						_tmp4 := dict
						_, _tmp5 := _tmp4[_tmp3]
						if _tmp5 {
							_tmp6 := candidate
							_tmp7 := visited
							_, _tmp8 := _tmp7[_tmp6]
							if !(_tmp8) {
								visited[candidate] = step
								next = append(append([]string{}, next...), []string{candidate}...)
							}
							if (visited[candidate] == step) {
								_tmp9 := candidate
								_tmp10 := parents
								_, _tmp11 := _tmp10[_tmp9]
								if _tmp11 {
									parents[candidate] = append(append([]string{}, parents[candidate]...), []string{word}...)
								} else {
									parents[candidate] = []string{word}
								}
							}
							if (candidate == endWord) {
								found = true
							}
						}
					}
					j = (j + 1)
				}
				i = (i + 1)
			}
		}
		queue = next
	}
	if !found {
		return _cast[[][]string]([]any{})
	}
	var results [][]string = [][]string{}
	var rev = func(lst []string) []string {
		var out []string = []string{}
		var i int = (len(lst) - 1)
		for (i >= 0) {
			out = append(append([]string{}, out...), []string{lst[i]}...)
			i = (i - 1)
		}
		return out
}
	var backtrack func(string, []string)
	backtrack = func(word string, path []string) {
		if (word == beginWord) {
			results = append(append([][]string{}, results...), [][]string{rev(append(append([]string{}, path...), []string{word}...))}...)
		} else {
			var ps []string = parents[word]
			for _, p := range ps {
				backtrack(p, append(append([]string{}, path...), []string{word}...))
			}
		}
}
	backtrack(endWord, []string{})
	return results
}

func example_1() {
	var res [][]string = findLadders("hit", "cog", []string{"hot", "dot", "dog", "lot", "log", "cog"})
	var sorted [][]string = func() [][]string {
	items := [][]string{}
	for _, r := range res {
		items = append(items, r)
	}
	type pair struct { item []string; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		r := it
		pairs[idx] = pair{item: it, key: r[2]}
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
	for _, r := range items {
		_res = append(_res, r)
	}
	return _res
}()
	_ = sorted
	var expected [][]string = [][]string{[]string{"hit", "hot", "dot", "dog", "cog"}, []string{"hit", "hot", "lot", "log", "cog"}}
	var expSorted [][]string = func() [][]string {
	items := [][]string{}
	for _, r := range expected {
		items = append(items, r)
	}
	type pair struct { item []string; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		r := it
		pairs[idx] = pair{item: it, key: r[2]}
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
	for _, r := range items {
		_res = append(_res, r)
	}
	return _res
}()
	_ = expSorted
	expect(_equal(sorted, expSorted))
}

func example_2() {
	expect(_equal(findLadders("hit", "cog", []string{"hot", "dot", "dog", "lot", "log"}), []any{}))
}

func main() {
	example_1()
	example_2()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
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

