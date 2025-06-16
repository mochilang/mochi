package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func lengthOfLongestSubstring(s string) int {
	var n int = len(s)
	var start int = 0
	var best int = 0
	var i int = 0
	for (i < n) {
		var j int = start
		for (j < i) {
			if (_indexString(s, j) == _indexString(s, i)) {
				start = (j + 1)
				break
			}
			j = (j + 1)
		}
		var length int = ((i - start) + 1)
		if (length > best) {
			best = length
		}
		i = (i + 1)
	}
	return best
}

func example_1() {
	expect((lengthOfLongestSubstring("abcabcbb") == 3))
}

func example_2() {
	expect((lengthOfLongestSubstring("bbbbb") == 1))
}

func example_3() {
	expect((lengthOfLongestSubstring("pwwkew") == 3))
}

func empty_string() {
	expect((lengthOfLongestSubstring("") == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty_string()
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

