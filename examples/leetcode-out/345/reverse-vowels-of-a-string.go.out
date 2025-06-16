package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func reverseVowels(s string) string {
	var vowels map[string]bool = map[string]bool{"a": true, "e": true, "i": true, "o": true, "u": true, "A": true, "E": true, "I": true, "O": true, "U": true}
	var chars []string = []string{}
	for _, r := range []rune(s) {
		ch := string(r)
		chars = append(append([]string{}, chars...), []string{ch}...)
	}
	var i int = 0
	var j int = (len(chars) - 1)
	for (i < j) {
		for (i < j) {
			_tmp0 := chars[i]
			_tmp1 := vowels
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				break
			}
			i = (i + 1)
		}
		for (i < j) {
			_tmp3 := chars[j]
			_tmp4 := vowels
			_, _tmp5 := _tmp4[_tmp3]
			if _tmp5 {
				break
			}
			j = (j - 1)
		}
		if (i < j) {
			var tmp string = chars[i]
			chars[i] = chars[j]
			chars[j] = tmp
			i = (i + 1)
			j = (j - 1)
		}
	}
	var result string = ""
	for _, ch := range chars {
		result = result + ch
	}
	return result
}

func example_1() {
	expect((reverseVowels("hello") == "holle"))
}

func example_2() {
	expect((reverseVowels("leetcode") == "leotcede"))
}

func mixed_case() {
	expect((reverseVowels("aA") == "Aa"))
}

func empty_string() {
	expect((reverseVowels("") == ""))
}

func main() {
	example_1()
	example_2()
	mixed_case()
	empty_string()
}

