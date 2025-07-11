//go:build ignore

package main

import (
	"fmt"
)

// line 4
func amb(wordsets [][]string, res *[]string, idx int) bool {
	if idx == len(wordsets) {
		return true
	}
	var prev string = ""
	if idx > 0 {
		prev = res[(idx - 1)]
	}
	var i int = 0
	for {
		if !(i < len(wordsets[idx])) {
			break
		}
		var w string = wordsets[idx][i]
		if (idx == 0) || (_sliceString(prev, (len(prev)-1), len(prev)) == _sliceString(w, 0, 1)) {
			res[idx] = w
			if amb(wordsets, res, (idx + 1)) {
				return true
			}
		}
		i = (i + 1)
	}
	return false
}

// line 26
func main() {
	var wordset [][]string = [][]string{
		[]string{"the", "that", "a"},
		[]string{"frog", "elephant", "thing"},
		[]string{"walked", "treaded", "grows"},
		[]string{"slowly", "quickly"},
	}
	var res []string = []string{}
	var i int = 0
	for {
		if !(i < len(wordset)) {
			break
		}
		res = append(_convSlice[string, any](res), "")
		i = (i + 1)
	}
	if amb(wordset, res, 0) {
		var out string = "[" + res[0]
		var j int = 1
		for {
			if !(j < len(res)) {
				break
			}
			out = out + " " + res[j]
			j = (j + 1)
		}
		out = out + "]"
		fmt.Println(out)
	} else {
		fmt.Println("No amb found")
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
