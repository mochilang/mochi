package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPalindrome(x int) bool {
	if (x < 0) {
		return false
	}
	var s string = fmt.Sprint(x)
	var n int = len(s)
	for i := 0; i < (n / 2); i++ {
		if (_indexString(s, i) != _indexString(s, ((n - 1) - i))) {
			return false
		}
	}
	return true
}

func example_1() {
	expect((isPalindrome(121) == true))
}

func example_2() {
	expect((isPalindrome(-121) == false))
}

func example_3() {
	expect((isPalindrome(10) == false))
}

func zero() {
	expect((isPalindrome(0) == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	zero()
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

