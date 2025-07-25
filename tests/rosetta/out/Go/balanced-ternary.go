//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 4
func trimLeftZeros(s string) string {
	i := 0
	for (i < len(any(s))) && (string([]rune(s)[i:(i+1)]) == "0") {
		i = (i + 1)
	}
	return string([]rune(s)[i:len(any(s))])
}

// line 12
func btString(s string) map[string]any {
	s = trimLeftZeros(s)
	var b []int = []int{}
	i := (len(any(s)) - 1)
	for i >= 0 {
		ch := string([]rune(s)[i:(i + 1)])
		if ch == "+" {
			b = append(_toAnySlice(b), any(1))
		} else {
			if ch == "0" {
				b = append(_toAnySlice(b), any(0))
			} else {
				if ch == "-" {
					b = append(_toAnySlice(b), any((0 - 1)))
				} else {
					return map[string]any{"bt": []any{}, "ok": false}
				}
			}
		}
		i = (i - 1)
	}
	return map[string]any{"bt": b, "ok": true}
}

// line 36
func btToString(b []int) string {
	if len(any(b)) == 0 {
		return "0"
	}
	r := ""
	i := (len(any(b)) - 1)
	for i >= 0 {
		d := b[i]
		if d == (0 - 1) {
			r = r + "-"
		} else {
			if d == 0 {
				r = r + "0"
			} else {
				r = r + "+"
			}
		}
		i = (i - 1)
	}
	return r
}

// line 56
func btInt(i int) []int {
	if i == 0 {
		return []int{}
	}
	n := i
	var b []int = []int{}
	for n != 0 {
		m := (n % 3)
		n = int((float64(n) / float64(3)))
		if m == 2 {
			m = (0 - 1)
			n = (n + 1)
		} else {
			if m == (0 - 2) {
				m = 1
				n = (n - 1)
			}
		}
		b = append(_toAnySlice(b), any(m))
	}
	return b
}

// line 77
func btToInt(b []int) int {
	r := 0
	pt := 1
	i := 0
	for i < len(any(b)) {
		r = (r + (b[i] * pt))
		pt = (pt * 3)
		i = (i + 1)
	}
	return r
}

// line 89
func btNeg(b []int) []int {
	var r []int = []int{}
	i := 0
	for i < len(any(b)) {
		r = append(_toAnySlice(r), any(-b[i]))
		i = (i + 1)
	}
	return r
}

// line 99
func btAdd(a []int, b []int) []int {
	return btInt((btToInt(a) + btToInt(b)))
}

// line 103
func btMul(a []int, b []int) []int {
	return btInt((btToInt(a) * btToInt(b)))
}

// line 107
func padLeft(s string, w int) string {
	r := s
	for len(any(r)) < w {
		r = " " + r
	}
	return r
}

// line 113
func show(label string, b []int) {
	l := padLeft(label, 7)
	bs := padLeft(btToString(b), 12)
	is := padLeft(fmt.Sprint(any(btToInt(b))), 7)
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(l+" "+bs+" "+is)), "\n"))
}

// line 120
func main() {
	ares := btString("+-0++0+")
	a := ares["bt"]
	b := btInt(-436)
	cres := btString("+-++-")
	c := cres["bt"]
	show("a:", (a).([]int))
	show("b:", b)
	show("c:", (c).([]int))
	show("a(b-c):", btMul((a).([]int), btAdd(b, btNeg((c).([]int)))))
}

func main() {
	main()
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
