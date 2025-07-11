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
	for _, n := range []int{
		0,
		1,
		2,
		3,
		4,
		5,
		10,
		40,
		-1,
	} {
		if n < 0 {
			fmt.Println("fib undefined for negative numbers")
		} else {
			fmt.Println("fib " + fmt.Sprint(n) + " = " + fmt.Sprint(fib(n)))
		}
	}
}

func main() {
	main()
}
