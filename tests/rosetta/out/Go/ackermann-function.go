//go:build ignore

package main

import (
	"fmt"
)

// line 4
func ackermann(m int, n int) int {
	if m == 0 {
		return (n + 1)
	}
	if n == 0 {
		return ackermann((m - 1), 1)
	}
	return ackermann((m - 1), ackermann(m, (n-1)))
}

// line 14
func main() {
	fmt.Println("A(0, 0) = " + fmt.Sprint(ackermann(0, 0)))
	fmt.Println("A(1, 2) = " + fmt.Sprint(ackermann(1, 2)))
	fmt.Println("A(2, 4) = " + fmt.Sprint(ackermann(2, 4)))
	fmt.Println("A(3, 4) = " + fmt.Sprint(ackermann(3, 4)))
}

func main() {
	main()
}
