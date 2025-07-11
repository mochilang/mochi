//go:build ignore

package main

import (
	"fmt"
)

// line 4
func fib(n int) int {
	if n < 2 {
		return n
	}
	var a int = 0
	var b int = 1
	var i int = 1
	for {
		if !(i < n) {
			break
		}
		var t int = (a + b)
		a = b
		b = t
		i = (i + 1)
	}
	return b
}

// line 18
func main() {
	for _, i := range []int{
		-1,
		0,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		10,
	} {
		if i < 0 {
			fmt.Println("fib(" + fmt.Sprint(i) + ") returned error: negative n is forbidden")
		} else {
			fmt.Println("fib(" + fmt.Sprint(i) + ") = " + fmt.Sprint(fib(i)))
		}
	}
}

func main() {
	main()
}
