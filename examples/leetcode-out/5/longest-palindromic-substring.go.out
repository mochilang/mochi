package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func expand(s string, left int, right int) int {
	var l int = left
	var r int = right
	var n int = len(s)
	for ((l >= 0) && (r < n)) {
		if (_indexString(s, l) != _indexString(s, r)) {
			break
		}
		l = (l - 1)
		r = (r + 1)
	}
	return ((r - l) - 1)
}

func longestPalindrome(s string) string {
	if (len(s) <= 1) {
		return s
	}
	var start int = 0
	var end int = 0
	var n int = len(s)
	for i := 0; i < n; i++ {
		var len1 int = expand(s, i, i)
		var len2 int = expand(s, i, (i + 1))
		var l int = len1
		if (len2 > len1) {
			l = len2
		}
		if (l > (end - start)) {
			start = (i - (((l - 1)) / 2))
			end = (i + (l / 2))
		}
	}
	return string([]rune(s)[start:(end + 1)])
}

func example_1() {
	var ans string = longestPalindrome("babad")
	_ = ans
	expect(((ans == "bab") || (ans == "aba")))
}

func example_2() {
	expect((longestPalindrome("cbbd") == "bb"))
}

func single_char() {
	expect((longestPalindrome("a") == "a"))
}

func two_chars() {
	var ans string = longestPalindrome("ac")
	_ = ans
	expect(((ans == "a") || (ans == "c")))
}

func main() {
	example_1()
	example_2()
	single_char()
	two_chars()
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

