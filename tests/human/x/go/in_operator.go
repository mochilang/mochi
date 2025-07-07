//go:build ignore

package main

import "fmt"

func containsInt(slice []int, val int) bool {
	for _, x := range slice {
		if x == val {
			return true
		}
	}
	return false
}

func main() {
	xs := []int{1, 2, 3}
	fmt.Println(containsInt(xs, 2))
	fmt.Println(!containsInt(xs, 5))
}
