package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func strStr(haystack string, needle string) int {
	var n int = len(haystack)
	var m int = len(needle)
	if (m == 0) {
		return 0
	}
	if (m > n) {
		return -1
	}
	for i := 0; i < ((n - m) + 1); i++ {
		var j int = 0
		for (j < m) {
			if (_indexString(haystack, (i + j)) != _indexString(needle, j)) {
				break
			}
			j = (j + 1)
		}
		if (j == m) {
			return i
		}
	}
	return -1
}

func example_1() {
	expect((strStr("sadbutsad", "sad") == 0))
}

func example_2() {
	expect((strStr("leetcode", "leeto") == (-1)))
}

func empty_needle() {
	expect((strStr("abc", "") == 0))
}

func needle_at_end() {
	expect((strStr("hello", "lo") == 3))
}

func main() {
	example_1()
	example_2()
	empty_needle()
	needle_at_end()
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

