package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isOneEditDistance(s string, t string) bool {
	var m int = len(s)
	var n int = len(t)
	if (m > n) {
		return isOneEditDistance(t, s)
	}
	if ((n - m) > 1) {
		return false
	}
	var i int = 0
	for (i < m) {
		if (_indexString(s, i) != _indexString(t, i)) {
			if (m == n) {
				return (string([]rune(s)[(i + 1):m]) == string([]rune(t)[(i + 1):n]))
			}
			return (string([]rune(s)[i:m]) == string([]rune(t)[(i + 1):n]))
		}
		i = (i + 1)
	}
	return ((n - m) == 1)
}

func example_1() {
	expect((isOneEditDistance("ab", "acb") == true))
}

func example_2() {
	expect((isOneEditDistance("cab", "ad") == false))
}

func example_3() {
	expect((isOneEditDistance("1203", "1213") == true))
}

func identical() {
	expect((isOneEditDistance("a", "a") == false))
}

func insert_at_end() {
	expect((isOneEditDistance("abc", "abcc") == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	identical()
	insert_at_end()
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

