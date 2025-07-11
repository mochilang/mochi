//go:build ignore

package main

import (
	"fmt"
	"strings"
)

// line 2
func fields(s string) []string {
	var res []string = []string{}
	var cur string = ""
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		var c string = _sliceString(s, i, (i + 1))
		if c == " " {
			if len(cur) > 0 {
				res = append(_convSlice[string, any](res), cur)
				cur = ""
			}
		} else {
			cur = cur + c
		}
		i = (i + 1)
	}
	if len(cur) > 0 {
		res = append(_convSlice[string, any](res), cur)
	}
	return res
}

// line 24
func canSpell(word string, blks []string) bool {
	if len(word) == 0 {
		return true
	}
	var c string = strings.ToLower(_sliceString(word, 0, 1))
	var i int = 0
	for {
		if !(i < len(blks)) {
			break
		}
		var b string = blks[i]
		if (c == strings.ToLower(_sliceString(b, 0, 1))) || (c == strings.ToLower(_sliceString(b, 1, 2))) {
			var rest []string = []string{}
			var j int = 0
			for {
				if !(j < len(blks)) {
					break
				}
				if j != i {
					rest = append(_convSlice[string, any](rest), blks[j])
				}
				j = (j + 1)
			}
			if canSpell(_sliceString(word, 1, len([]rune(word))), rest) {
				return true
			}
		}
		i = (i + 1)
	}
	return false
}

// line 44
func newSpeller(blocks string) func(string) bool {
	var bl []string = fields(blocks)
	return func(w string) bool {
		return canSpell(w, bl)
	}
}

// line 49
func main() {
	var sp func(string) bool = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")
	for _, word := range []string{
		"A",
		"BARK",
		"BOOK",
		"TREAT",
		"COMMON",
		"SQUAD",
		"CONFUSE",
	} {
		fmt.Println(word + " " + fmt.Sprint(sp(word)))
	}
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
