package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func bucketID(x, size int64) int64 {
	if x >= 0 {
		return x / size
	}
	return -((-x-1)/size) - 1
}

func solve(nums []int64, indexDiff int, valueDiff int64) bool {
	size := valueDiff + 1
	buckets := map[int64]int64{}
	for i, x := range nums {
		bid := bucketID(x, size)
		if _, ok := buckets[bid]; ok {
			return true
		}
		if y, ok := buckets[bid-1]; ok && abs64(x-y) <= valueDiff {
			return true
		}
		if y, ok := buckets[bid+1]; ok && abs64(x-y) <= valueDiff {
			return true
		}
		buckets[bid] = x
		if i >= indexDiff {
			old := nums[i-indexDiff]
			delete(buckets, bucketID(old, size))
		}
	}
	return false
}

func abs64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	next := func() string { sc.Scan(); return sc.Text() }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		n, _ := strconv.Atoi(next())
		nums := make([]int64, n)
		for i := 0; i < n; i++ {
			v, _ := strconv.ParseInt(next(), 10, 64)
			nums[i] = v
		}
		indexDiff, _ := strconv.Atoi(next())
		valueDiff, _ := strconv.ParseInt(next(), 10, 64)
		if tc > 0 {
			fmt.Fprintln(w)
		}
		if solve(nums, indexDiff, valueDiff) {
			fmt.Fprint(w, "true")
		} else {
			fmt.Fprint(w, "false")
		}
	}
}
