//go:build ignore

package main

import (
	"fmt"
)

// line 1
func twoSum(nums []int, target int) []int {
	var n int = len(nums)
	for i := 0; i < n; i++ {
		for j := (i + 1); j < n; j++ {
			if (nums[i] + nums[j]) == target {
				return []int{i, j}
			}
		}
	}
	return []int{-1, -1}
}

func main() {
	var result []int = twoSum([]int{
		2,
		7,
		11,
		15,
	}, 9)
	fmt.Println(result[0])
	fmt.Println(result[1])
}
