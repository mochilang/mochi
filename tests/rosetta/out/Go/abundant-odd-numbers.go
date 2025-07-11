//go:build ignore

package main

import (
	"encoding/json"
	"fmt"

	"golang.org/x/exp/constraints"
)

// line 4
func divisors(n int) []int {
	var divs []int = []int{1}
	var divs2 []int = []int{}
	var i int = 2
	for {
		if !((i * i) <= n) {
			break
		}
		if (n % i) == 0 {
			var j int = _cast[int]((float64(n) / float64(i)))
			divs = append(_convSlice[int, any](divs), i)
			if i != j {
				divs2 = append(_convSlice[int, any](divs2), j)
			}
		}
		i = (i + 1)
	}
	var j int = (len(divs2) - 1)
	for {
		if !(j >= 0) {
			break
		}
		divs = append(_convSlice[int, any](divs), divs2[j])
		j = (j - 1)
	}
	return divs
}

// line 26
func sum(xs []int) int {
	var tot int = 0
	for _, v := range xs {
		tot = (tot + v)
	}
	return tot
}

// line 34
func sumStr(xs []int) string {
	var s string = ""
	var i int = 0
	for {
		if !(i < len(xs)) {
			break
		}
		s = s + fmt.Sprint(xs[i]) + " + "
		i = (i + 1)
	}
	return _sliceString(s, 0, (len(s) - 3))
}

// line 44
func pad2(n int) string {
	var s string = fmt.Sprint(n)
	if len(s) < 2 {
		return " " + s
	}
	return s
}

// line 50
func pad5(n int) string {
	var s string = fmt.Sprint(n)
	for {
		if !(len(s) < 5) {
			break
		}
		s = " " + s
	}
	return s
}

// line 58
func abundantOdd(searchFrom int, countFrom int, countTo int, printOne bool) int {
	var count int = countFrom
	var n int = searchFrom
	for {
		if !(count < countTo) {
			break
		}
		var divs []int = divisors(n)
		var tot int = _sumOrdered[int](divs)
		if tot > n {
			count = (count + 1)
			if printOne && (count < countTo) {
				n = (n + 2)
				continue
			}
			var s string = sumStr(divs)
			if !printOne {
				fmt.Println(pad2(count) + ". " + pad5(n) + " < " + s + " = " + fmt.Sprint(tot))
			} else {
				fmt.Println(fmt.Sprint(n) + " < " + s + " = " + fmt.Sprint(tot))
			}
		}
		n = (n + 2)
	}
	return n
}

// line 82
func main() {
	var max int = 25
	fmt.Println("The first " + "25" + " abundant odd numbers are:")
	var n int = abundantOdd(1, 0, max, false)
	fmt.Println("\nThe one thousandth abundant odd number is:")
	abundantOdd(n, max, 1000, true)
	fmt.Println("\nThe first abundant odd number above one billion is:")
	abundantOdd(1000000001, 0, 1, true)
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

func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum
}
