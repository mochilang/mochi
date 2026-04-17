package main

import (
	"bufio"
	"fmt"
	"os"
)

func isValid(s string) bool {
	stack := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		ch := s[i]
		if ch == '(' || ch == '[' || ch == '{' {
			stack = append(stack, ch)
		} else {
			if len(stack) == 0 {
				return false
			}
			open := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if (ch == ')' && open != '(') || (ch == ']' && open != '[') || (ch == '}' && open != '{') {
				return false
			}
		}
	}
	return len(stack) == 0
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for i := 0; i < t; i++ {
		var s string
		fmt.Fscan(in, &s)
		if isValid(s) {
			fmt.Fprintln(out, "true")
		} else {
			fmt.Fprintln(out, "false")
		}
	}
}
