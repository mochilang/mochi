//go:build ignore

package main

import "fmt"

func main() {
	nums := []int{1, 2, 3}
	sum := 0
	for _, n := range nums {
		sum += n
	}
	fmt.Println(sum)
}
