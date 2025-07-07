//go:build ignore

package main

import "fmt"

func main() {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9}
	for _, n := range numbers {
		if n%2 == 0 {
			continue
		}
		if n > 7 {
			break
		}
		fmt.Println("odd number:", n)
	}
}
