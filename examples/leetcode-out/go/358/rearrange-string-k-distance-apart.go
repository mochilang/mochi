package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func rearrangeString(s string, k int) string {
	if (k <= 1) {
		return s
	}
	var counts map[string]int = map[string]int{}
	var i int = 0
	for (i < len(s)) {
		var ch string = _indexString(s, i)
		_tmp0 := ch
		_tmp1 := counts
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			counts[ch] = (counts[ch] + 1)
		} else {
			counts[ch] = 1
		}
		i = (i + 1)
	}
	var cooldown map[string]int = map[string]int{}
	var result string = ""
	var step int = 0
	var n int = len(s)
	for (len(result) < n) {
		var bestChar string = ""
		var bestCount int = -1
		for ch := range counts {
			var remaining int = counts[ch]
			var next int = 0
			_tmp3 := ch
			_tmp4 := cooldown
			_, _tmp5 := _tmp4[_tmp3]
			if _tmp5 {
				next = cooldown[ch]
			}
			if ((remaining > 0) && (next <= step)) {
				if (remaining > bestCount) {
					bestCount = remaining
					bestChar = ch
				}
			}
		}
		if (bestCount == (-1)) {
			return ""
		}
		result = result + bestChar
		counts[bestChar] = (counts[bestChar] - 1)
		cooldown[bestChar] = (step + k)
		step = (step + 1)
	}
	return result
}

func sortString(t string) string {
	var chars []string = []string{}
	var i int = 0
	for (i < len(t)) {
		chars = append(append([]string{}, chars...), []string{_indexString(t, i)}...)
		i = (i + 1)
	}
	var sorted []string = func() []string {
	items := []string{}
	for _, c := range chars {
		items = append(items, c)
	}
	type pair struct { item string; key any }
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
	_res := []string{}
	for _, c := range items {
		_res = append(_res, c)
	}
	return _res
}()
	var out string = ""
	for _, c := range sorted {
		out = out + c
	}
	return out
}

func isValid(result string, original string, k int) bool {
	if (result == "") {
		return true
	}
	if (sortString(result) != sortString(original)) {
		return false
	}
	var pos map[string]int = map[string]int{}
	var i int = 0
	for (i < len(result)) {
		var ch string = _indexString(result, i)
		_tmp6 := ch
		_tmp7 := pos
		_, _tmp8 := _tmp7[_tmp6]
		if _tmp8 {
			if ((i - pos[ch]) < k) {
				return false
			}
		}
		pos[ch] = i
		i = (i + 1)
	}
	return true
}

func example_1() {
	var res string = rearrangeString("aabbcc", 3)
	_ = res
	expect(isValid(res, "aabbcc", 3))
}

func example_2() {
	expect((rearrangeString("aaabc", 3) == ""))
}

func example_3() {
	var res string = rearrangeString("aaadbbcc", 2)
	_ = res
	expect(isValid(res, "aaadbbcc", 2))
}

func k_one() {
	var res string = rearrangeString("aab", 1)
	_ = res
	expect((res == "aab"))
}

func single_char() {
	expect((rearrangeString("a", 2) == "a"))
}

func impossible() {
	expect((rearrangeString("aaa", 2) == ""))
}

func main() {
	example_1()
	example_2()
	example_3()
	k_one()
	single_char()
	impossible()
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

