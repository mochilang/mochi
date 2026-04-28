package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(n int) int {
	if n == 0 {
		return 0
	}
	digits := make([]byte, 0)
	for n > 0 {
		digits = append(digits, byte('0'+n%9))
		n /= 9
	}
	for i, j := 0, len(digits)-1; i < j; i, j = i+1, j-1 {
		digits[i], digits[j] = digits[j], digits[i]
	}
	ans, _ := strconv.Atoi(string(digits))
	return ans
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for i := 0; i < t; i++ {
		var n int
		fmt.Fscan(in, &n)
		if i > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(n))
	}
}
