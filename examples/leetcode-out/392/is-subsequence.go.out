package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isSubsequence(s string, t string) bool {
	var n int = len(s)
	var m int = len(t)
	var i int = 0
	var j int = 0
	for ((i < n) && (j < m)) {
		if (_indexString(s, i) == _indexString(t, j)) {
			i = (i + 1)
		}
		j = (j + 1)
	}
	return (i == n)
}

func example_1() {
	expect((isSubsequence("abc", "ahbgdc") == true))
}

func example_2() {
	expect((isSubsequence("axc", "ahbgdc") == false))
}

func empty_s() {
	expect((isSubsequence("", "ahbgdc") == true))
}

func empty_t() {
	expect((isSubsequence("a", "") == false))
}

func both_empty() {
	expect((isSubsequence("", "") == true))
}

func main() {
	example_1()
	example_2()
	empty_s()
	empty_t()
	both_empty()
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

