//go:build ignore

package main

import (
	"fmt"
)

// line 1
func nextRand(seed int) int {
	return (((seed * 1664525) + 1013904223) % 2147483647)
}

// line 5
func shuffleChars(s string, seed int) []any {
	var chars []string = []string{}
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		chars = append(_convSlice[string, any](chars), _sliceString(s, i, (i+1)))
		i = (i + 1)
	}
	var sd int = seed
	var idx int = (len(chars) - 1)
	for {
		if !(idx > 0) {
			break
		}
		sd = nextRand(sd)
		var j int = (sd % (idx + 1))
		var tmp string = chars[idx]
		chars[idx] = chars[j]
		chars[j] = tmp
		idx = (idx - 1)
	}
	var res string = ""
	i = 0
	for {
		if !(i < len(chars)) {
			break
		}
		res = res + chars[i]
		i = (i + 1)
	}
	return []any{res, sd}
}

// line 31
func bestShuffle(s string, seed int) []any {
	var r []any = shuffleChars(s, seed)
	var t any = r[0]
	var sd any = r[1]
	var arr []string = []string{}
	var i int = 0
	for {
		if !(i < len(t)) {
			break
		}
		arr = append(_convSlice[string, any](arr), _sliceString((t).(string), i, (i+1)))
		i = (i + 1)
	}
	i = 0
	for {
		if !(i < len(arr)) {
			break
		}
		var j int = 0
		for {
			if !(j < len(arr)) {
				break
			}
			if ((i != j) && (arr[i] != _sliceString(s, j, (j+1)))) && (arr[j] != _sliceString(s, i, (i+1))) {
				var tmp string = arr[i]
				arr[i] = arr[j]
				arr[j] = tmp
				break
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	var count int = 0
	i = 0
	for {
		if !(i < len(arr)) {
			break
		}
		if arr[i] == _sliceString(s, i, (i+1)) {
			count = (count + 1)
		}
		i = (i + 1)
	}
	var out string = ""
	i = 0
	for {
		if !(i < len(arr)) {
			break
		}
		out = out + arr[i]
		i = (i + 1)
	}
	return []any{out, sd, count}
}

// line 72
func main() {
	var ts []string = []string{
		"abracadabra",
		"seesaw",
		"elk",
		"grrrrrr",
		"up",
		"a",
	}
	var seed int = 1
	var i int = 0
	for {
		if !(i < len(ts)) {
			break
		}
		var r []any = bestShuffle(ts[i], seed)
		var shuf any = r[0]
		seed = (((r[1]).(int)).(int)).(int)
		var cnt any = r[2]
		fmt.Println(ts[i] + " -> " + fmt.Sprint(shuf) + " (" + fmt.Sprint(cnt) + ")")
		i = (i + 1)
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
