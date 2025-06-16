package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func removeDuplicateLetters(s string) string {
	var last map[string]int = map[string]int{}
	var i int = 0
	for (i < len(s)) {
		last[_indexString(s, i)] = i
		i = (i + 1)
	}
	var stack []string = []string{}
	var seen map[string]bool = map[string]bool{}
	i = 0
	for (i < len(s)) {
		var c string = _indexString(s, i)
		var present bool = false
		_tmp0 := c
		_tmp1 := seen
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			present = seen[c]
		}
		if !present {
			for (len(stack) > 0) {
				var top string = stack[(len(stack) - 1)]
				if ((top > c) && (last[top] > i)) {
					stack = stack[0:(len(stack) - 1)]
					seen[top] = false
				} else {
					break
				}
			}
			stack = append(append([]string{}, stack...), []string{c}...)
			seen[c] = true
		}
		i = (i + 1)
	}
	var result string = ""
	for _, ch := range stack {
		result = result + ch
	}
	return result
}

func example_1() {
	expect((removeDuplicateLetters("bcabc") == "abc"))
}

func example_2() {
	expect((removeDuplicateLetters("cbacdcbc") == "acdb"))
}

func repeated_letters() {
	expect((removeDuplicateLetters("aaaa") == "a"))
}

func leetcode() {
	expect((removeDuplicateLetters("leetcode") == "letcod"))
}

func mixed_order() {
	expect((removeDuplicateLetters("ecbacba") == "eacb"))
}

func main() {
	example_1()
	example_2()
	repeated_letters()
	leetcode()
	mixed_order()
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

