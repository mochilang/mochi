package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func findMin(nums []int) int {
	left, right := 0, len(nums)-1
	for left < right {
		mid := (left + right) / 2
		if nums[mid] < nums[right] {
			right = mid
		} else if nums[mid] > nums[right] {
			left = mid + 1
		} else {
			right--
		}
	}
	return nums[left]
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	var lines []string
	for in.Scan() {
		lines = append(lines, in.Text())
	}
	if len(lines) == 0 {
		return
	}
	tc, _ := strconv.Atoi(lines[0])
	idx := 1
	out := make([]string, 0, tc)
	for t := 0; t < tc; t++ {
		n, _ := strconv.Atoi(lines[idx])
		idx++
		nums := make([]int, n)
		for i := 0; i < n; i++ {
			nums[i], _ = strconv.Atoi(lines[idx])
			idx++
		}
		out = append(out, strconv.Itoa(findMin(nums)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
