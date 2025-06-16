package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func ord(ch string) int {
	var letters map[string]int = map[string]int{"a": 0, "b": 1, "c": 2, "d": 3, "e": 4, "f": 5, "g": 6, "h": 7, "i": 8, "j": 9, "k": 10, "l": 11, "m": 12, "n": 13, "o": 14, "p": 15, "q": 16, "r": 17, "s": 18, "t": 19, "u": 20, "v": 21, "w": 22, "x": 23, "y": 24, "z": 25}
	_tmp0 := ch
	_tmp1 := letters
	_, _tmp2 := _tmp1[_tmp0]
	if _tmp2 {
		return letters[ch]
	}
	return 0
}

func patternKey(s string) string {
	if (len(s) == 0) {
		return ""
	}
	var key string = ""
	var base int = ord(_indexString(s, 0))
	var i int = 0
	for (i < len(s)) {
		var diff int = ((((ord(_indexString(s, i)) - base) + 26)) % 26)
		key = key + fmt.Sprint(diff) + ","
		i = (i + 1)
	}
	return key
}

func groupStrings(strings []string) [][]string {
	var groups map[string][]string = map[string][]string{}
	for _, s := range strings {
		var k string = patternKey(s)
		var lst []string = []string{}
		_tmp3 := k
		_tmp4 := groups
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			lst = groups[k]
		}
		lst = append(append([]string{}, lst...), []string{s}...)
		groups[k] = lst
	}
	var result [][]string = [][]string{}
	for k := range groups {
		result = append(append([][]string{}, result...), [][]string{groups[k]}...)
	}
	return result
}

func example_1() {
	var input []string = []string{"abc", "bcd", "acef", "xyz", "az", "ba", "a", "z"}
	var res [][]string = groupStrings(input)
	_ = res
	expect((len(res) == 4))
}

func single() {
	expect((groupStrings([]string{"a"})[0][0] == "a"))
}

func empty_list() {
	var res [][]string = groupStrings([]string{})
	_ = res
	expect((len(res) == 0))
}

func main() {
	example_1()
	single()
	empty_list()
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

