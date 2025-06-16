package main

import (
	"fmt"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func generateAbbreviations(word string) []string {
	var result []string = []string{}
	var backtrack func(int, string, int)
	backtrack = func(pos int, cur string, count int) {
		if (pos == len(word)) {
			var tmp string = cur
			if (count > 0) {
				tmp = tmp + fmt.Sprint(count)
			}
			result = append(append([]string{}, result...), []string{tmp}...)
		} else {
			backtrack((pos + 1), cur, (count + 1))
			var next string = cur
			if (count > 0) {
				next = next + fmt.Sprint(count)
			}
			next = next + _indexString(word, pos)
			backtrack((pos + 1), next, 0)
		}
}
	backtrack(0, "", 0)
	return result
}

func example_1() {
	expect(_equal(generateAbbreviations("word"), []string{"4", "3d", "2r1", "2rd", "1o2", "1o1d", "1or1", "1ord", "w3", "w2d", "w1r1", "w1rd", "wo2", "wo1d", "wor1", "word"}))
}

func empty_string() {
	expect(_equal(generateAbbreviations(""), []string{""}))
}

func main() {
	example_1()
	empty_string()
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
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

