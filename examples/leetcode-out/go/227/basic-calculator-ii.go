package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func calculate(s string) int {
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var stack []int = []int{}
	var current int = 0
	var op string = "+"
	var i int = 0
	var n int = len(s)
	for (i <= n) {
		var ch string = " "
		if (i < n) {
			ch = _indexString(s, i)
		}
		_tmp0 := ch
		_tmp1 := digits
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			current = ((current * 10) + digits[ch])
		}
		_tmp3 := ch
		_tmp4 := digits
		_, _tmp5 := _tmp4[_tmp3]
		if (((!(_tmp5) && (ch != " "))) || (i == n)) {
			if (op == "+") {
				stack = append(append([]int{}, stack...), []int{current}...)
			} else 			if (op == "-") {
				stack = append(append([]int{}, stack...), []int{-current}...)
			} else 			if (op == "*") {
				var last int = stack[(len(stack) - 1)]
				stack = stack[0:(len(stack) - 1)]
				stack = append(append([]int{}, stack...), []int{(last * current)}...)
			} else {
				var last int = stack[(len(stack) - 1)]
				stack = stack[0:(len(stack) - 1)]
				stack = append(append([]int{}, stack...), []int{(last / current)}...)
			}
			op = ch
			current = 0
		}
		i = (i + 1)
	}
	var result int = 0
	for _, num := range stack {
		result = (result + num)
	}
	return result
}

func example_1() {
	expect((calculate("3+2*2") == 7))
}

func example_2() {
	expect((calculate(" 3/2 ") == 1))
}

func example_3() {
	expect((calculate(" 3+5 / 2 ") == 5))
}

func mix_operations() {
	expect((calculate("14-3/2") == 13))
}

func single_number() {
	expect((calculate("0") == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
	mix_operations()
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

