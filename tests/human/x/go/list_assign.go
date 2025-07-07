//go:build ignore

package main

import "fmt"

func main() {
	nums := []int{1, 2}
	nums[1] = 3
	fmt.Println(nums[1])
}
