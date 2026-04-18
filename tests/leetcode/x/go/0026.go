package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func removeDuplicates(nums []int) int {
	if len(nums) == 0 {
		return 0
	}
	k := 1
	for i := 1; i < len(nums); i++ {
		if nums[i] != nums[k-1] {
			nums[k] = nums[i]
			k++
		}
	}
	return k
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	next := func() string {
		if scanner.Scan() {
			return scanner.Text()
		}
		return ""
	}

	tStr := next()
	if tStr == "" {
		return
	}
	var t int
	fmt.Sscanf(tStr, "%d", &t)

	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Sscanf(next(), "%d", &n)
		nums := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Sscanf(next(), "%d", &nums[i])
		}
		k := removeDuplicates(nums)
		res := make([]string, k)
		for i := 0; i < k; i++ {
			res[i] = fmt.Sprintf("%d", nums[i])
		}
		fmt.Println(strings.Join(res, " "))
	}
}
