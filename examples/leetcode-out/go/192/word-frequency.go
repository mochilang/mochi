package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type WordCount struct {
	Word string `json:"word"`
	Count int `json:"count"`
}

func wordFrequency(lines []string) []WordCount {
	var counts map[string]int = map[string]int{}
	for _, line := range lines {
		var i int = 0
		var word string = ""
		var n int = len(line)
		for (i <= n) {
			var ch string = ""
			if (i < n) {
				ch = _indexString(line, i)
			} else {
				ch = " "
			}
			if (ch == " ") {
				if (word != "") {
					var c int = 0
					_tmp0 := word
					_tmp1 := counts
					_, _tmp2 := _tmp1[_tmp0]
					if _tmp2 {
						c = counts[word]
					}
					counts[word] = (c + 1)
					word = ""
				}
			} else {
				word = word + ch
			}
			i = (i + 1)
		}
	}
	var result []WordCount = []WordCount{}
	for w := range counts {
		result = append(append([]WordCount{}, result...), []WordCount{WordCount{Word: w, Count: counts[w]}}...)
	}
	var alpha []WordCount = func() []WordCount {
	items := []WordCount{}
	for _, wc := range result {
		items = append(items, wc)
	}
	type pair struct { item WordCount; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		wc := it
		pairs[idx] = pair{item: it, key: wc.Word}
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
	_res := []WordCount{}
	for _, wc := range items {
		_res = append(_res, wc)
	}
	return _res
}()
	var sorted []WordCount = func() []WordCount {
	items := []WordCount{}
	for _, wc := range alpha {
		items = append(items, wc)
	}
	type pair struct { item WordCount; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		wc := it
		pairs[idx] = pair{item: it, key: -wc.Count}
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
	_res := []WordCount{}
	for _, wc := range items {
		_res = append(_res, wc)
	}
	return _res
}()
	return sorted
}

func example() {
	var lines []string = []string{"the day is sunny the the the sunny is is"}
	var res []WordCount = wordFrequency(lines)
	var e0 WordCount = res[0]
	_ = e0
	expect((e0.Word == "the"))
	expect((e0.Count == 4))
	var e1 WordCount = res[1]
	_ = e1
	expect((e1.Word == "is"))
	expect((e1.Count == 3))
	var e2 WordCount = res[2]
	_ = e2
	expect((e2.Word == "sunny"))
	expect((e2.Count == 2))
	var e3 WordCount = res[3]
	_ = e3
	expect((e3.Word == "day"))
	expect((e3.Count == 1))
}

func multiple_lines() {
	var lines []string = []string{"hello world", "hello mochi world"}
	var res []WordCount = wordFrequency(lines)
	var a0 WordCount = res[0]
	_ = a0
	expect((a0.Word == "hello"))
	expect((a0.Count == 2))
	var a1 WordCount = res[1]
	_ = a1
	expect((a1.Word == "world"))
	expect((a1.Count == 2))
	var a2 WordCount = res[2]
	_ = a2
	expect((a2.Word == "mochi"))
	expect((a2.Count == 1))
}

func main() {
	example()
	multiple_lines()
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

