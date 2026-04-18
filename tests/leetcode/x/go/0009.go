package main

import (
	"bufio"
	"fmt"
	"os"
)

func isPalindrome(x int) bool {
	if x < 0 {
		return false
	}
	original := x
	rev := 0
	for x > 0 {
		rev = rev*10 + x%10
		x /= 10
	}
	return rev == original
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
		var x int
		fmt.Fscan(in, &x)
		if isPalindrome(x) {
			fmt.Fprintln(out, "true")
		} else {
			fmt.Fprintln(out, "false")
		}
	}
}
