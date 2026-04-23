package main

import (
	"bufio"
	"fmt"
	"os"
)

func minPatches(nums []int, n int) int {
	miss := int64(1)
	i, patches := 0, 0
	for miss <= int64(n) {
		if i < len(nums) && int64(nums[i]) <= miss {
			miss += int64(nums[i]); i++
		} else {
			miss += miss; patches++
		}
	}
	return patches
}

func main() {
	in := bufio.NewReader(os.Stdin); out := bufio.NewWriter(os.Stdout); defer out.Flush()
	var t int; if _, err := fmt.Fscan(in, &t); err != nil { return }
	for tc := 0; tc < t; tc++ {
		var size int; fmt.Fscan(in, &size); nums := make([]int, size)
		for i := range nums { fmt.Fscan(in, &nums[i]) }
		var n int; fmt.Fscan(in, &n)
		if tc > 0 { fmt.Fprintln(out); fmt.Fprintln(out) }
		fmt.Fprint(out, minPatches(nums, n))
	}
}
