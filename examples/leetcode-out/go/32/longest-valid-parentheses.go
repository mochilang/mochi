package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func longestValidParentheses(s string) int {
	var n int = len(s)
	var stack []int = []int{}
	var best int = 0
	var last int = -1
	for i := 0; i < n; i++ {
		var c string = _indexString(s, i)
		if (c == "(") {
			stack = append(append([]int{}, stack...), []int{i}...)
		} else {
			if (len(stack) == 0) {
				last = i
			} else {
				stack = stack[0:(len(stack) - 1)]
				if (len(stack) == 0) {
					var length int = (i - last)
					if (length > best) {
						best = length
					}
				} else {
					var length int = (i - stack[(len(stack) - 1)])
					if (length > best) {
						best = length
					}
				}
			}
		}
	}
	return best
}

func example_1() {
	expect((longestValidParentheses("(()") == 2))
}

func example_2() {
	expect((longestValidParentheses(")()())") == 4))
}

func example_3() {
	expect((longestValidParentheses("") == 0))
}

func all_open() {
	expect((longestValidParentheses("(((") == 0))
}

func balanced() {
	expect((longestValidParentheses("()()") == 4))
}

func main() {
	example_1()
	example_2()
	example_3()
	all_open()
	balanced()
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

