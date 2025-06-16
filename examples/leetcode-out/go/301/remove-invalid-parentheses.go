package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isValid(s string) bool {
	var count int = 0
	var i int = 0
	for (i < len(s)) {
		var c string = _indexString(s, i)
		if (c == "(") {
			count = (count + 1)
		} else 		if (c == ")") {
			if (count == 0) {
				return false
			}
			count = (count - 1)
		}
		i = (i + 1)
	}
	return (count == 0)
}

func removeInvalidParentheses(s string) []string {
	var result []string = []string{}
	var visited map[string]bool = map[string]bool{}
	var queue []string = []string{s}
	visited[s] = true
	var found bool = false
	for (len(queue) > 0) {
		var cur string = queue[0]
		queue = queue[1:len(queue)]
		if isValid(cur) {
			result = append(append([]string{}, result...), []string{cur}...)
			found = true
		}
		if found {
			continue
		}
		var i int = 0
		for (i < len(cur)) {
			var ch string = _indexString(cur, i)
			if ((ch != "(") && (ch != ")")) {
				i = (i + 1)
				continue
			}
			var next string = string([]rune(cur)[0:i]) + string([]rune(cur)[(i + 1):len(cur)])
			_tmp0 := next
			_tmp1 := visited
			_, _tmp2 := _tmp1[_tmp0]
			if !(_tmp2) {
				queue = append(append([]string{}, queue...), []string{next}...)
				visited[next] = true
			}
			i = (i + 1)
		}
	}
	if (len(result) == 0) {
		return []string{""}
	}
	return result
}

func example_1() {
	expect(_equal(removeInvalidParentheses("()())()"), []string{"(())()", "()()()"}))
}

func example_2() {
	expect(_equal(removeInvalidParentheses("(a)())()"), []string{"(a())()", "(a)()()"}))
}

func example_3() {
	expect(_equal(removeInvalidParentheses(")("), []string{""}))
}

func already_valid() {
	expect(_equal(removeInvalidParentheses("(a)(b)"), []string{"(a)(b)"}))
}

func empty() {
	expect(_equal(removeInvalidParentheses(""), []string{""}))
}

func main() {
	example_1()
	example_2()
	example_3()
	already_valid()
	empty()
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

