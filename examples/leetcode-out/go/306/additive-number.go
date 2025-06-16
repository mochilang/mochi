package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func addStrings(a string, b string) string {
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var i int = len(a)
	var j int = len(b)
	var carry int = 0
	var out string = ""
	for (((i > 0) || (j > 0)) || (carry > 0)) {
		var x int = 0
		if (i > 0) {
			i = (i - 1)
			x = digits[_indexString(a, i)]
		}
		var y int = 0
		if (j > 0) {
			j = (j - 1)
			y = digits[_indexString(b, j)]
		}
		var sum int = ((x + y) + carry)
		carry = (sum / 10)
		var digit int = (sum % 10)
		out = fmt.Sprint(digit) + out
	}
	return out
}

func isAdditiveNumber(num string) bool {
	var n int = len(num)
	var i int = 1
	for (i <= (n - 2)) {
		if ((_indexString(num, 0) == "0") && (i > 1)) {
			break
		}
		var j int = (i + 1)
		for (j <= (n - 1)) {
			if ((_indexString(num, i) == "0") && ((j - i) > 1)) {
				break
			}
			var first string = string([]rune(num)[0:i])
			var second string = string([]rune(num)[i:j])
			var k int = j
			for (k < n) {
				var third string = addStrings(first, second)
				var lenThird int = len(third)
				if ((k + lenThird) > n) {
					break
				}
				if (string([]rune(num)[k:(k + lenThird)]) != third) {
					break
				}
				first = second
				second = third
				k = (k + lenThird)
			}
			if (k == n) {
				return true
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return false
}

func example_1() {
	expect((isAdditiveNumber("112358") == true))
}

func example_2() {
	expect((isAdditiveNumber("199100199") == true))
}

func leading_zero() {
	expect((isAdditiveNumber("1023") == false))
}

func all_zeros() {
	expect((isAdditiveNumber("000") == true))
}

func short_sequence() {
	expect((isAdditiveNumber("101") == true))
}

func main() {
	example_1()
	example_2()
	leading_zero()
	all_zeros()
	short_sequence()
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

