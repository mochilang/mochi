package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func parseInt(s string) int {
	var i int = 0
	var sign int = 1
	if ((len(s) > 0) && (((_indexString(s, 0) == "-") || (_indexString(s, 0) == "+")))) {
		if (_indexString(s, 0) == "-") {
			sign = -1
		}
		i = 1
	}
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var result int = 0
	for (i < len(s)) {
		var ch string = _indexString(s, i)
		result = ((result * 10) + digits[ch])
		i = (i + 1)
	}
	return (result * sign)
}

func evalRPN(tokens []string) int {
	var stack []int = []int{}
	for _, tok := range tokens {
		if ((((tok == "+") || (tok == "-")) || (tok == "*")) || (tok == "/")) {
			var b int = stack[(len(stack) - 1)]
			stack = stack[0:(len(stack) - 1)]
			var a int = stack[(len(stack) - 1)]
			stack = stack[0:(len(stack) - 1)]
			if (tok == "+") {
				stack = append(append([]int{}, stack...), []int{(a + b)}...)
			} else 			if (tok == "-") {
				stack = append(append([]int{}, stack...), []int{(a - b)}...)
			} else 			if (tok == "*") {
				stack = append(append([]int{}, stack...), []int{(a * b)}...)
			} else {
				stack = append(append([]int{}, stack...), []int{(a / b)}...)
			}
		} else {
			var val int = parseInt(tok)
			stack = append(append([]int{}, stack...), []int{val}...)
		}
	}
	return stack[(len(stack) - 1)]
}

func example_1() {
	expect((evalRPN([]string{"2", "1", "+", "3", "*"}) == 9))
}

func example_2() {
	expect((evalRPN([]string{"4", "13", "5", "/", "+"}) == 6))
}

func example_3() {
	expect((evalRPN([]string{"10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"}) == 22))
}

func single_number() {
	expect((evalRPN([]string{"42"}) == 42))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_number()
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

