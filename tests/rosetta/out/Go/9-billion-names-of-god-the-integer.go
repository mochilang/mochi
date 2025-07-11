//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 3
func bigTrim(a []int) []int {
	var n int = len(a)
	for {
		if !((n > 1) && (a[(n-1)] == 0)) {
			break
		}
		a = a[0:(n - 1)]
		n = (n - 1)
	}
	return a
}

// line 12
func bigFromInt(x int) []int {
	if x == 0 {
		return []int{0}
	}
	var digits []int = []int{}
	var n int = x
	for {
		if !(n > 0) {
			break
		}
		digits = append(_convSlice[int, any](digits), (n % 10))
		n = int(int(_cast[int]((float64(n) / float64(10)))))
	}
	return digits
}

// line 23
func bigAdd(a []int, b []int) []int {
	var res []int = []int{}
	var carry int = 0
	var i int = 0
	for {
		if !(((i < len(a)) || (i < len(b))) || (carry > 0)) {
			break
		}
		var av int = 0
		if i < len(a) {
			av = a[i]
		}
		var bv int = 0
		if i < len(b) {
			bv = b[i]
		}
		var s int = ((av + bv) + carry)
		res = append(_convSlice[int, any](res), (s % 10))
		carry = int(int(_cast[int]((float64(s) / float64(10)))))
		i = (i + 1)
	}
	return bigTrim(res)
}

// line 40
func bigSub(a []int, b []int) []int {
	var res []int = []int{}
	var borrow int = 0
	var i int = 0
	for {
		if !(i < len(a)) {
			break
		}
		var av int = a[i]
		var bv int = 0
		if i < len(b) {
			bv = b[i]
		}
		var diff int = ((av - bv) - borrow)
		if diff < 0 {
			diff = (diff + 10)
			borrow = 1
		} else {
			borrow = 0
		}
		res = append(_convSlice[int, any](res), diff)
		i = (i + 1)
	}
	return bigTrim(res)
}

// line 61
func bigToString(a []int) string {
	var s string = ""
	var i int = (len(a) - 1)
	for {
		if !(i >= 0) {
			break
		}
		s = s + fmt.Sprint(a[i])
		i = (i - 1)
	}
	return s
}

// line 71
func minInt(a int, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

// line 75
func cumu(n int) [][]int {
	var cache [][][]int = [][][]int{[][]int{bigFromInt(1)}}
	var y int = 1
	for {
		if !(y <= n) {
			break
		}
		var row [][]int = [][]int{bigFromInt(0)}
		var x int = 1
		for {
			if !(x <= y) {
				break
			}
			var val []int = cache[(y - x)][minInt(x, (y-x))]
			row = append(_convSlice[[]int, any](row), bigAdd(row[(len(row)-1)], val))
			x = (x + 1)
		}
		cache = append(_convSlice[[][]int, any](cache), row)
		y = (y + 1)
	}
	return cache[n]
}

// line 92
func row(n int) []string {
	var e [][]int = cumu(n)
	var out []string = []string{}
	var i int = 0
	for {
		if !(i < n) {
			break
		}
		var diff []int = bigSub(e[(i+1)], e[i])
		out = append(_convSlice[string, any](out), bigToString(diff))
		i = (i + 1)
	}
	return out
}

var x int

func main() {
	x = 1
	fmt.Println("rows:")
	for {
		if !(x < 11) {
			break
		}
		var r []string = row(x)
		var line string = ""
		var i int = 0
		for {
			if !(i < len(r)) {
				break
			}
			line = line + " " + r[i] + " "
			i = (i + 1)
		}
		fmt.Println(line)
		x = (x + 1)
	}
	fmt.Println("")
	fmt.Println("sums:")
	for _, num := range []int{23, 123, 1234} {
		var r [][]int = cumu(num)
		fmt.Println(fmt.Sprint(num) + " " + bigToString(r[(len(r)-1)]))
	}
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
