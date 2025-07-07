//go:build ignore

package main

import "fmt"

func min(slice []int) int {
	if len(slice) == 0 {
		panic("empty slice")
	}
	m := slice[0]
	for _, v := range slice[1:] {
		if v < m {
			m = v
		}
	}
	return m
}

func max(slice []int) int {
	if len(slice) == 0 {
		panic("empty slice")
	}
	m := slice[0]
	for _, v := range slice[1:] {
		if v > m {
			m = v
		}
	}
	return m
}

func main() {
	nums := []int{3, 1, 4}
	fmt.Println(min(nums))
	fmt.Println(max(nums))
}
