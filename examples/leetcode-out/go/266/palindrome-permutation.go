package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canPermutePalindrome(s string) bool {
	var counts map[string]int = map[string]int{}
	var i int = 0
	for (i < len(s)) {
		var ch string = _indexString(s, i)
		_tmp0 := ch
		_tmp1 := counts
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			counts[ch] = (counts[ch] + 1)
		} else {
			counts[ch] = 1
		}
		i = (i + 1)
	}
	var oddCount int = 0
	for key := range counts {
		if ((counts[key] % 2) == 1) {
			oddCount = (oddCount + 1)
			if (oddCount > 1) {
				return false
			}
		}
	}
	return true
}

func example_1() {
	expect((canPermutePalindrome("code") == false))
}

func example_2() {
	expect((canPermutePalindrome("aab") == true))
}

func example_3() {
	expect((canPermutePalindrome("carerac") == true))
}

func empty_string() {
	expect((canPermutePalindrome("") == true))
}

func single_char() {
	expect((canPermutePalindrome("a") == true))
}

func two_odds() {
	expect((canPermutePalindrome("abc") == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty_string()
	single_char()
	two_odds()
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

