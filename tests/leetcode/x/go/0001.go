package main

import (
	"bufio"
	"fmt"
	"os"
)

func twoSum(nums []int, target int) (int, int) {
	for i := 0; i < len(nums); i++ {
		for j := i + 1; j < len(nums); j++ {
			if nums[i]+nums[j] == target {
				return i, j
			}
		}
	}
	return 0, 0
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		var n, target int
		fmt.Fscan(in, &n, &target)
		nums := make([]int, n)
		for i := range nums {
			fmt.Fscan(in, &nums[i])
		}
		a, b := twoSum(nums, target)
		fmt.Fprintf(out, "%d %d\n", a, b)
	}
}
