package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isDigit(ch string) bool {
	return ((ch >= "0") && (ch <= "9"))
}

func isValidPhoneNumber(s string) bool {
	var n int = len(s)
	if (n == 12) {
		if ((_indexString(s, 3) != "-") || (_indexString(s, 7) != "-")) {
			return false
		}
		var i int = 0
		for (i < n) {
			if ((i == 3) || (i == 7)) {
				i = (i + 1)
				continue
			}
			if !isDigit(_indexString(s, i)) {
				return false
			}
			i = (i + 1)
		}
		return true
	}
	if (n == 14) {
		if ((((_indexString(s, 0) != "(") || (_indexString(s, 4) != ")")) || (_indexString(s, 5) != " ")) || (_indexString(s, 9) != "-")) {
			return false
		}
		var i int = 0
		for (i < n) {
			if ((((i == 0) || (i == 4)) || (i == 5)) || (i == 9)) {
				i = (i + 1)
				continue
			}
			if !isDigit(_indexString(s, i)) {
				return false
			}
			i = (i + 1)
		}
		return true
	}
	return false
}

func validPhoneNumbers(lines []string) []string {
	var result []string = []string{}
	for _, line := range lines {
		if isValidPhoneNumber(line) {
			result = append(append([]string{}, result...), []string{line}...)
		}
	}
	return result
}

func example() {
	var input []string = []string{"987-123-4567", "123 456 7890", "(123) 456-7890"}
	_ = input
	expect(_equal(validPhoneNumbers(input), []string{"987-123-4567", "(123) 456-7890"}))
}

func main() {
	example()
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

