//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 1
func fields(s string) []string {
	var words []string = []string{}
	var cur string = ""
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		var ch string = _sliceString(s, i, (i + 1))
		if ((ch == " ") || (ch == "\n")) || (ch == "\t") {
			if len(cur) > 0 {
				words = append(_convSlice[string, any](words), cur)
				cur = ""
			}
		} else {
			cur = cur + ch
		}
		i = (i + 1)
	}
	if len(cur) > 0 {
		words = append(_convSlice[string, any](words), cur)
	}
	return words
}

// line 23
func join(xs []string, sep string) string {
	var res string = ""
	var i int = 0
	for {
		if !(i < len(xs)) {
			break
		}
		if i > 0 {
			res = res + sep
		}
		res = res + xs[i]
		i = (i + 1)
	}
	return res
}

// line 36
func numberName(n int) string {
	var small []string = []string{
		"no",
		"one",
		"two",
		"three",
		"four",
		"five",
		"six",
		"seven",
		"eight",
		"nine",
		"ten",
		"eleven",
		"twelve",
		"thirteen",
		"fourteen",
		"fifteen",
		"sixteen",
		"seventeen",
		"eighteen",
		"nineteen",
	}
	var tens []string = []string{
		"ones",
		"ten",
		"twenty",
		"thirty",
		"forty",
		"fifty",
		"sixty",
		"seventy",
		"eighty",
		"ninety",
	}
	if n < 0 {
		return ""
	}
	if n < 20 {
		return small[n]
	}
	if n < 100 {
		var t string = tens[_cast[int]((float64(n) / float64(10)))]
		var s int = (n % 10)
		if s > 0 {
			t = t + " " + small[s]
		}
		return t
	}
	return ""
}

// line 59
func pluralizeFirst(s string, n int) string {
	if n == 1 {
		return s
	}
	var w []string = fields(s)
	if len(w) > 0 {
		w[0] = w[0] + "s"
	}
	return join(w, " ")
}

// line 67
func randInt(seed int, n int) int {
	var next int = (((seed * 1664525) + 1013904223) % 2147483647)
	return (next % n)
}

// line 72
func slur(p string, d int) string {
	if len(p) <= 2 {
		return p
	}
	var a []string = []string{}
	var i int = 1
	for {
		if !(i < (len(p) - 1)) {
			break
		}
		a = append(_convSlice[string, any](a), _sliceString(p, i, (i+1)))
		i = (i + 1)
	}
	var idx int = (len(a) - 1)
	var seed int = d
	for {
		if !(idx >= 1) {
			break
		}
		seed = (((seed * 1664525) + 1013904223) % 2147483647)
		if (seed % 100) >= d {
			var j int = (seed % (idx + 1))
			var tmp string = a[idx]
			a[idx] = a[j]
			a[j] = tmp
		}
		idx = (idx - 1)
	}
	var s string = _sliceString(p, 0, 1)
	var k int = 0
	for {
		if !(k < len(a)) {
			break
		}
		s = s + a[k]
		k = (k + 1)
	}
	s = s + _sliceString(p, (len(p)-1), len(p))
	var w []string = fields(s)
	return join(w, " ")
}

// line 103
func main() {
	var i int = 99
	for {
		if !(i > 0) {
			break
		}
		fmt.Println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer on the wall", i))
		fmt.Println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer", i))
		fmt.Println(slur("take one", i) + " " + slur("down", i) + " " + slur("pass it around", i))
		fmt.Println(slur(numberName((i-1)), i) + " " + pluralizeFirst(slur("bottle of", i), (i-1)) + " " + slur("beer on the wall", i))
		i = (i - 1)
	}
}

func main() {
	main()
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

func _sliceString(s string, i, j int) string {
	start := i
	end := j
	n := len([]rune(s))
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if end < start {
		end = start
	}
	return string([]rune(s)[start:end])
}
