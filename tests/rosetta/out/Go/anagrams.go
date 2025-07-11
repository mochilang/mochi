//go:build ignore

package main

import (
	"fmt"
)

// line 1
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

// line 31
func sortStrings(xs []string) []string {
	var res []string = []string{}
	var tmp []string = xs
	for {
		if !(len(tmp) > 0) {
			break
		}
		var min string = tmp[0]
		var idx int = 0
		var i int = 1
		for {
			if !(i < len(tmp)) {
				break
			}
			if tmp[i] < min {
				min = tmp[i]
				idx = i
			}
			i = (i + 1)
		}
		res = append(_convSlice[string, any](res), min)
		var out []string = []string{}
		var j int = 0
		for {
			if !(j < len(tmp)) {
				break
			}
			if j != idx {
				out = append(_convSlice[string, any](out), tmp[j])
			}
			j = (j + 1)
		}
		tmp = out
	}
	return res
}

// line 59
func main() {
	var words []string = []string{
		"abel",
		"able",
		"bale",
		"bela",
		"elba",
		"alger",
		"glare",
		"lager",
		"large",
		"regal",
		"angel",
		"angle",
		"galen",
		"glean",
		"lange",
		"caret",
		"carte",
		"cater",
		"crate",
		"trace",
		"elan",
		"lane",
		"lean",
		"lena",
		"neal",
		"evil",
		"levi",
		"live",
		"veil",
		"vile",
	}
	var groups map[string][]string = map[string][]string{}
	var maxLen int = 0
	for _, w := range words {
		var k string = sortRunes(w)
		_tmp0 := k
		_tmp1 := groups
		_, _tmp2 := _tmp1[_tmp0]
		if !(_tmp2) {
			groups[k] = []string{w}
		} else {
			groups[k] = append(_convSlice[string, any](groups[k]), w)
		}
		if len(groups[k]) > maxLen {
			maxLen = len(groups[k])
		}
	}
	var printed map[string]bool = map[string]bool{}
	for _, w := range words {
		var k string = sortRunes(w)
		if len(groups[k]) == maxLen {
			_tmp3 := k
			_tmp4 := printed
			_, _tmp5 := _tmp4[_tmp3]
			if !(_tmp5) {
				var g []string = sortStrings(groups[k])
				var line string = "[" + g[0]
				var i int = 1
				for {
					if !(i < len(g)) {
						break
					}
					line = line + " " + g[i]
					i = (i + 1)
				}
				line = line + "]"
				fmt.Println(line)
				printed[k] = true
			}
		}
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
