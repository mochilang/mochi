//go:build slow && ignore

// Solution for SPOJ ONP - Transform the Expression
// https://www.spoj.com/problems/ONP/
package main

import (
	"bufio"
	"fmt"
	"os"
)

func precedence(op rune) int {
	switch op {
	case '+', '-':
		return 1
	case '*', '/':
		return 2
	case '^':
		return 3
	default:
		return 0
	}
}

func isLetter(ch rune) bool {
	return ch >= 'a' && ch <= 'z'
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	var t int
	fmt.Fscan(in, &t)
	for ; t > 0; t-- {
		var expr string
		fmt.Fscan(in, &expr)
		stack := []rune{}
		for _, ch := range expr {
			switch {
			case isLetter(ch):
				fmt.Fprint(out, string(ch))
			case ch == '(':
				stack = append(stack, ch)
			case ch == ')':
				for len(stack) > 0 && stack[len(stack)-1] != '(' {
					out.WriteRune(stack[len(stack)-1])
					stack = stack[:len(stack)-1]
				}
				if len(stack) > 0 {
					stack = stack[:len(stack)-1]
				}
			default: // operator
				for len(stack) > 0 {
					top := stack[len(stack)-1]
					if top == '(' {
						break
					}
					if precedence(top) > precedence(ch) || (precedence(top) == precedence(ch) && ch != '^') {
						out.WriteRune(top)
						stack = stack[:len(stack)-1]
					} else {
						break
					}
				}
				stack = append(stack, ch)
			}
		}
		for i := len(stack) - 1; i >= 0; i-- {
			if stack[i] != '(' {
				out.WriteRune(stack[i])
			}
		}
		if t > 1 {
			out.WriteByte('\n')
		}
	}
	out.Flush()
}
