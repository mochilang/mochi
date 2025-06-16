package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func buildSet(word string) map[string]bool {
	var m map[string]bool = map[string]bool{}
	for _, r := range []rune(word) {
		ch := string(r)
		m[ch] = true
	}
	return m
}

func shareLetters(a map[string]bool, b map[string]bool) bool {
	for ch := range a {
		_tmp0 := ch
		_tmp1 := b
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			return true
		}
	}
	return false
}

func maxProduct(words []string) int {
	var n int = len(words)
	var sets []map[string]bool = []map[string]bool{}
	var i int = 0
	for (i < n) {
		sets = append(append([]map[string]bool{}, sets...), []map[string]bool{buildSet(words[i])}...)
		i = (i + 1)
	}
	var best int = 0
	i = 0
	for (i < n) {
		var j int = (i + 1)
		for (j < n) {
			if !shareLetters(sets[i], sets[j]) {
				var prod int = (len(words[i]) * len(words[j]))
				if (prod > best) {
					best = prod
				}
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return best
}

func example_1() {
	expect((maxProduct([]string{"abcw", "baz", "foo", "bar", "xtfn", "abcdef"}) == 16))
}

func example_2() {
	expect((maxProduct([]string{"a", "ab", "abc", "d", "cd", "bcd", "abcd"}) == 4))
}

func example_3() {
	expect((maxProduct([]string{"a", "aa", "aaa", "aaaa"}) == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
}

