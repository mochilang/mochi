package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPalindrome(s string) bool {
	var i int = 0
	var j int = (len(s) - 1)
	for (i < j) {
		if (_indexString(s, i) != _indexString(s, j)) {
			return false
		}
		i = (i + 1)
		j = (j - 1)
	}
	return true
}

func shortestPalindrome(s string) string {
	var n int = len(s)
	var i int = n
	for (i > 0) {
		if isPalindrome(string([]rune(s)[0:i])) {
			var suffix string = string([]rune(s)[i:n])
			var rev string = ""
			var k int = (len(suffix) - 1)
			for (k >= 0) {
				rev = rev + _indexString(suffix, k)
				k = (k - 1)
			}
			return rev + s
		}
		i = (i - 1)
	}
	return s
}

func example_1() {
	expect((shortestPalindrome("aacecaaa") == "aaacecaaa"))
}

func example_2() {
	expect((shortestPalindrome("abcd") == "dcbabcd"))
}

func empty() {
	expect((shortestPalindrome("") == ""))
}

func already_palindrome() {
	expect((shortestPalindrome("aba") == "aba"))
}

func single_char() {
	expect((shortestPalindrome("a") == "a"))
}

func main() {
	example_1()
	example_2()
	empty()
	already_palindrome()
	single_char()
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

