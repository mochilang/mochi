package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func decodeString(s string) string {
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var counts []int = []int{}
	var strs []string = []string{}
	var curr string = ""
	var num int = 0
	var i int = 0
	var n int = len(s)
	for (i < n) {
		var c string = _indexString(s, i)
		if ((c >= "0") && (c <= "9")) {
			num = ((num * 10) + digits[c])
		} else 		if (c == "[") {
			counts = append(append([]int{}, counts...), []int{num}...)
			strs = append(append([]string{}, strs...), []string{curr}...)
			curr = ""
			num = 0
		} else 		if (c == "]") {
			var repeat int = counts[(len(counts) - 1)]
			counts = counts[0:(len(counts) - 1)]
			var prev string = strs[(len(strs) - 1)]
			strs = strs[0:(len(strs) - 1)]
			var repeated string = ""
			var j int = 0
			for (j < repeat) {
				repeated = repeated + curr
				j = (j + 1)
			}
			curr = prev + repeated
		} else {
			curr = curr + c
		}
		i = (i + 1)
	}
	return curr
}

func example_1() {
	expect((decodeString("3[a]2[bc]") == "aaabcbc"))
}

func example_2() {
	expect((decodeString("3[a2[c]]") == "accaccacc"))
}

func example_3() {
	expect((decodeString("2[abc]3[cd]ef") == "abcabccdcdcdef"))
}

func plain_text_with_brackets() {
	expect((decodeString("abc3[cd]xyz") == "abccdcdcdxyz"))
}

func multi_digit_number() {
	expect((decodeString("10[a]") == "aaaaaaaaaa"))
}

func main() {
	example_1()
	example_2()
	example_3()
	plain_text_with_brackets()
	multi_digit_number()
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

