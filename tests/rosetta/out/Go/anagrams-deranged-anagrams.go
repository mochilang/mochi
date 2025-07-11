//go:build ignore

package main

import (
	"fmt"
)

// line 4
func sortRunes(s string) string {
	var arr []string = []string{}
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		arr = append(_convSlice[string, any](arr), _sliceString(s, i, (i+1)))
		i = (i + 1)
	}
	var n int = len(arr)
	var m int = 0
	for {
		if !(m < n) {
			break
		}
		var j int = 0
		for {
			if !(j < (n - 1)) {
				break
			}
			if arr[j] > arr[(j+1)] {
				var tmp string = arr[j]
				arr[j] = arr[(j + 1)]
				arr[(j + 1)] = tmp
			}
			j = (j + 1)
		}
		m = (m + 1)
	}
	var out string = ""
	i = 0
	for {
		if !(i < n) {
			break
		}
		out = out + arr[i]
		i = (i + 1)
	}
	return out
}

// line 34
func deranged(a string, b string) bool {
	if len(a) != len(b) {
		return false
	}
	var i int = 0
	for {
		if !(i < len(a)) {
			break
		}
		if _sliceString(a, i, (i+1)) == _sliceString(b, i, (i+1)) {
			return false
		}
		i = (i + 1)
	}
	return true
}

// line 44
func main() {
	var words []string = []string{"constitutionalism", "misconstitutional"}
	var m map[string][]string = map[string][]string{}
	var bestLen int = 0
	var w1 string = ""
	var w2 string = ""
	for _, w := range words {
		if len(w) <= bestLen {
			continue
		}
		var k string = sortRunes(w)
		_tmp0 := k
		_tmp1 := m
		_, _tmp2 := _tmp1[_tmp0]
		if !(_tmp2) {
			m[k] = []string{w}
			continue
		}
		for _, c := range m[k] {
			if deranged(w, c) {
				bestLen = len(w)
				w1 = c
				w2 = w
				break
			}
		}
		m[k] = append(_convSlice[string, any](m[k]), w)
	}
	fmt.Println(w1 + " " + w2 + " : Length " + fmt.Sprint(bestLen))
}

func main() {
	main()
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
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
