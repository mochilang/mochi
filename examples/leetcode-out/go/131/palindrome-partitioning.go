package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func partition(s string) [][]string {
	var n int = len(s)
	var result [][]string = [][]string{}
	var isPal = func(left int, right int) bool {
		var l int = left
		var r int = right
		for (l < r) {
			if (_indexString(s, l) != _indexString(s, r)) {
				return false
			}
			l = (l + 1)
			r = (r - 1)
		}
		return true
}
	var dfs func(int, []string)
	dfs = func(start int, path []string) {
		if (start == n) {
			result = append(append([][]string{}, result...), [][]string{path}...)
		} else {
			var end int = start
			for (end < n) {
				if isPal(start, end) {
					dfs((end + 1), append(append([]string{}, path...), []string{string([]rune(s)[start:(end + 1)])}...))
				}
				end = (end + 1)
			}
		}
}
	dfs(0, []string{})
	return result
}

func example_1() {
	expect(_equal(partition("aab"), [][]string{[]string{"a", "a", "b"}, []string{"aa", "b"}}))
}

func example_2() {
	expect(_equal(partition("a"), [][]string{[]string{"a"}}))
}

func no_palindrome() {
	expect(_equal(partition("abc"), [][]string{[]string{"a", "b", "c"}}))
}

func main() {
	example_1()
	example_2()
	no_palindrome()
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

