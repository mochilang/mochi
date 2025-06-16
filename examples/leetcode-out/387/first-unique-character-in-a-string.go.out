package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func firstUniqChar(s string) int {
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
	i = 0
	for (i < len(s)) {
		var ch string = _indexString(s, i)
		if (counts[ch] == 1) {
			return i
		}
		i = (i + 1)
	}
	return -1
}

func example_1() {
	expect((firstUniqChar("leetcode") == 0))
}

func example_2() {
	expect((firstUniqChar("loveleetcode") == 2))
}

func example_3() {
	expect((firstUniqChar("aabb") == (-1)))
}

func empty_string() {
	expect((firstUniqChar("") == (-1)))
}

func single_char() {
	expect((firstUniqChar("z") == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty_string()
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

