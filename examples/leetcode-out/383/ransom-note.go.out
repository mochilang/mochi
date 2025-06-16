package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canConstruct(ransomNote string, magazine string) bool {
	var counts map[string]int = map[string]int{}
	var i int = 0
	for (i < len(magazine)) {
		var ch string = _indexString(magazine, i)
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
	i = 0
	for (i < len(ransomNote)) {
		var ch string = _indexString(ransomNote, i)
		_tmp3 := ch
		_tmp4 := counts
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			counts[ch] = (counts[ch] - 1)
			if (counts[ch] < 0) {
				return false
			}
		} else {
			return false
		}
		i = (i + 1)
	}
	return true
}

func example_1() {
	expect((canConstruct("a", "b") == false))
}

func example_2() {
	expect((canConstruct("aa", "ab") == false))
}

func example_3() {
	expect((canConstruct("aa", "aab") == true))
}

func empty_ransom() {
	expect((canConstruct("", "abc") == true))
}

func not_enough_letters() {
	expect((canConstruct("abc", "ab") == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty_ransom()
	not_enough_letters()
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

