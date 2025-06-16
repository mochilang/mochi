package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func splitWords(s string) []string {
	var i int = 0
	var words []string = []string{}
	var current string = ""
	for (i < len(s)) {
		var c string = _indexString(s, i)
		if (c == " ") {
			if (len(current) > 0) {
				words = append(append([]string{}, words...), []string{current}...)
				current = ""
			}
		} else {
			current = current + c
		}
		i = (i + 1)
	}
	if (len(current) > 0) {
		words = append(append([]string{}, words...), []string{current}...)
	}
	return words
}

func wordPattern(pattern string, s string) bool {
	var words []string = splitWords(s)
	if (len(words) != len(pattern)) {
		return false
	}
	var p2w map[string]string = map[string]string{}
	var w2p map[string]string = map[string]string{}
	var i int = 0
	for (i < len(pattern)) {
		var pch string = _indexString(pattern, i)
		var word string = words[i]
		_tmp0 := pch
		_tmp1 := p2w
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			if (p2w[pch] != word) {
				return false
			}
		} else {
			p2w[pch] = word
		}
		_tmp3 := word
		_tmp4 := w2p
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			if (w2p[word] != pch) {
				return false
			}
		} else {
			w2p[word] = pch
		}
		i = (i + 1)
	}
	return true
}

func example_1() {
	expect((wordPattern("abba", "dog cat cat dog") == true))
}

func example_2() {
	expect((wordPattern("abba", "dog cat cat fish") == false))
}

func example_3() {
	expect((wordPattern("aaaa", "dog cat cat dog") == false))
}

func example_4() {
	expect((wordPattern("abba", "dog dog dog dog") == false))
}

func single_word() {
	expect((wordPattern("a", "dog") == true))
}

func mismatch_lengths() {
	expect((wordPattern("ab", "one") == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	example_4()
	single_word()
	mismatch_lengths()
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

