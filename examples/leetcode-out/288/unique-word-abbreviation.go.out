package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func abbrev(word string) string {
	var n int = len(word)
	if (n <= 2) {
		return word
	}
	return _indexString(word, 0) + fmt.Sprint((n - 2)) + _indexString(word, (n - 1))
}

func buildAbbrs(dict []string) map[string]map[string]bool {
	var table map[string]map[string]bool = map[string]map[string]bool{}
	for _, w := range dict {
		var a string = abbrev(w)
		var set map[string]bool = map[string]bool{}
		_tmp0 := a
		_tmp1 := table
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			set = table[a]
		}
		set[w] = true
		table[a] = set
	}
	return table
}

func isUnique(word string, table map[string]map[string]bool) bool {
	var a string = abbrev(word)
	_tmp3 := a
	_tmp4 := table
	_, _tmp5 := _tmp4[_tmp3]
	if !(_tmp5) {
		return true
	}
	var words map[string]bool = table[a]
	var count int = 0
	var exists bool = false
	for k := range words {
		count = (count + 1)
		if (k == word) {
			exists = true
		}
	}
	if ((count == 1) && exists) {
		return true
	}
	return false
}

func unique_1() {
	var sample map[string]map[string]bool = buildAbbrs([]string{"deer", "door", "cake", "card"})
	_ = sample
	expect((isUnique("dear", sample) == false))
}

func unique_2() {
	var sample map[string]map[string]bool = buildAbbrs([]string{"deer", "door", "cake", "card"})
	_ = sample
	expect((isUnique("cart", sample) == true))
}

func unique_3() {
	var sample map[string]map[string]bool = buildAbbrs([]string{"deer", "door", "cake", "card"})
	_ = sample
	expect((isUnique("cane", sample) == false))
}

func unique_4() {
	var sample map[string]map[string]bool = buildAbbrs([]string{"deer", "door", "cake", "card"})
	_ = sample
	expect((isUnique("make", sample) == true))
}

func main() {
	unique_1()
	unique_2()
	unique_3()
	unique_4()
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

