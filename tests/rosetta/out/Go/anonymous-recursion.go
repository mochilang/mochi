//go:build ignore

package main

import (
	"fmt"
)

// line 1
func fib(n int) int {
	if n < 2 {
		return n
	}
	return (fib((n - 1)) + fib((n - 2)))
}

// line 6
func main() {
	var i int = -1
	for {
		if !(i <= 10) {
			break
		}
		if i < 0 {
			fmt.Println("fib(" + fmt.Sprint(i) + ") returned error: negative n is forbidden")
		} else {
			fmt.Println("fib(" + fmt.Sprint(i) + ") = " + fmt.Sprint(fib(i)))
		}
		i = (i + 1)
	}
}

func main() {
	main()
}
