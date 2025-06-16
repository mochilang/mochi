package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func generatePalindromes(s string) []string {
	var letters []string = []string{}
	var i int = 0
	for (i < len(s)) {
		letters = append(append([]string{}, letters...), []string{_indexString(s, i)}...)
		i = (i + 1)
	}
	var counts []map[string]any = func() []map[string]any {
	_res := []map[string]any{}
	for _, ch := range letters {
		_res = append(_res, map[string]any{"ch": g.Key, "cnt": _count(g)})
	}
	return _res
}()
	var odd int = 0
	var center string = ""
	var half []string = []string{}
	for _, item := range counts {
		if ((_cast[int](item["cnt"]) % 2) == 1) {
			odd = (odd + 1)
			center = item["ch"]
		}
		var j int = 0
		for (j < (_cast[int](item["cnt"]) / 2)) {
			half = append(append([]string{}, half...), []string{item["ch"]}...)
			j = (j + 1)
		}
	}
	if (odd > 1) {
		return _cast[[]string]([]any{})
	}
	var arr []string = func() []string {
	items := []string{}
	for _, x := range half {
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
	var n int = len(arr)
	var result []string = []string{}
	var used map[int]bool = map[int]bool{}
	var backtrack func([]string)
	backtrack = func(path []string) {
		if (len(path) == n) {
			var left string = ""
			var k int = 0
			for (k < n) {
				left = left + path[k]
				k = (k + 1)
			}
			var right string = ""
			k = (n - 1)
			for (k >= 0) {
				right = right + path[k]
				k = (k - 1)
			}
			var pal string = left
			if (odd == 1) {
				pal = pal + center
			}
			pal = pal + right
			result = append(append([]string{}, result...), []string{pal}...)
		} else {
			var i int = 0
			for (i < n) {
				var curUsed bool = false
				_tmp0 := i
				_tmp1 := used
				_, _tmp2 := _tmp1[_tmp0]
				if _tmp2 {
					curUsed = used[i]
				}
				if curUsed {
					i = (i + 1)
					continue
				}
				if ((i > 0) && (arr[i] == arr[(i - 1)])) {
					var prevUsed bool = false
					_tmp3 := (i - 1)
					_tmp4 := used
					_, _tmp5 := _tmp4[_tmp3]
					if _tmp5 {
						prevUsed = used[(i - 1)]
					}
					if !prevUsed {
						i = (i + 1)
						continue
					}
				}
				used[i] = true
				backtrack(append(append([]string{}, path...), []string{arr[i]}...))
				used[i] = false
				i = (i + 1)
			}
		}
}
	backtrack([]string{})
	return result
}

func example_1() {
	var ans []string = generatePalindromes("aabb")
	var sorted []string = func() []string {
	items := []string{}
	for _, x := range ans {
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
	_ = sorted
	expect(_equal(sorted, []string{"abba", "baab"}))
}

func example_2() {
	expect(_equal(generatePalindromes("abc"), []any{}))
}

func single_char() {
	expect(_equal(generatePalindromes("a"), []string{"a"}))
}

func triple() {
	var res []string = generatePalindromes("aaa")
	_ = res
	expect(_equal(res, []string{"aaa"}))
}

func main() {
	example_1()
	example_2()
	single_char()
	triple()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

func _count(v any) int {
    if g, ok := v.(*data.Group); ok { return len(g.Items) }
    switch s := v.(type) {
    case []any: return len(s)
    case []int: return len(s)
    case []float64: return len(s)
    case []string: return len(s)
    case []bool: return len(s)
    case map[string]any: return len(s)
    case string: return len([]rune(s))
    default: panic("count() expects list or group")
    }
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

