package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func myAtoi(s string) int {
	var i int = 0
	var n int = len(s)
	for ((i < n) && (_indexString(s, i) == " ")) {
		i = (i + 1)
	}
	var sign int = 1
	if ((i < n) && (((_indexString(s, i) == "+") || (_indexString(s, i) == "-")))) {
		if (_indexString(s, i) == "-") {
			sign = -1
		}
		i = (i + 1)
	}
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var result int = 0
	for (i < n) {
		var ch string = _indexString(s, i)
		_tmp0 := ch
		_tmp1 := digits
		_, _tmp2 := _tmp1[_tmp0]
		if !(_tmp2) {
			break
		}
		var d int = digits[ch]
		result = ((result * 10) + d)
		i = (i + 1)
	}
	result = (result * sign)
	if (result > 2147483647) {
		return 2147483647
	}
	if (result < (-2147483648)) {
		return -2147483648
	}
	return result
}

func example_1() {
	expect((myAtoi("42") == 42))
}

func example_2() {
	expect((myAtoi("   -42") == (-42)))
}

func example_3() {
	expect((myAtoi("4193 with words") == 4193))
}

func example_4() {
	expect((myAtoi("words and 987") == 0))
}

func example_5() {
	expect((myAtoi("-91283472332") == (-2147483648)))
}

func main() {
	example_1()
	example_2()
	example_3()
	example_4()
	example_5()
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

