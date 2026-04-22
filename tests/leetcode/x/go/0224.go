package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func calculate(expr string) int {
	result, number, sign := 0, 0, 1
	stack := []int{}
	for _, ch := range expr {
		if ch >= '0' && ch <= '9' {
			number = number*10 + int(ch-'0')
		} else if ch == '+' || ch == '-' {
			result += sign * number
			number = 0
			if ch == '+' {
				sign = 1
			} else {
				sign = -1
			}
		} else if ch == '(' {
			stack = append(stack, result, sign)
			result, number, sign = 0, 0, 1
		} else if ch == ')' {
			result += sign * number
			number = 0
			n := len(stack)
			prevSign := stack[n-1]
			prevResult := stack[n-2]
			stack = stack[:n-2]
			result = prevResult + prevSign*result
		}
	}
	return result + sign*number
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	out := make([]int, 0, t)
	for i := 0; i < t && sc.Scan(); i++ {
		out = append(out, calculate(sc.Text()))
	}
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for i, v := range out {
		if i > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, v)
	}
}
