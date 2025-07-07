//go:build ignore

package main

import "fmt"

func sumRec(n, acc int) int {
	if n == 0 {
		return acc
	}
	return sumRec(n-1, acc+n)
}

func main() {
	fmt.Println(sumRec(10, 0))
}
