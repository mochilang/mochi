package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findTheDifference(s string, t string) string {
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
	for (i < len(t)) {
		var ch string = _indexString(t, i)
		_tmp3 := ch
		_tmp4 := counts
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			counts[ch] = (counts[ch] - 1)
		} else {
			counts[ch] = -1
		}
		i = (i + 1)
	}
	for key := range counts {
		if (counts[key] != 0) {
			return key
		}
	}
	return ""
}

func example_1() {
	expect((findTheDifference("abcd", "abcde") == "e"))
}

func example_2() {
	expect((findTheDifference("", "y") == "y"))
}

func extra_repeated() {
	expect((findTheDifference("aabb", "aabbb") == "b"))
}

func extra_at_beginning() {
	expect((findTheDifference("ae", "aea") == "a"))
}

func main() {
	example_1()
	example_2()
	extra_repeated()
	extra_at_beginning()
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

