package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

// Problem: Crime at Piccadilly Circus
// https://www.spoj.com/problems/PICAD/

// solve returns the minimal and maximal number of people present simultaneously
// at the crime scene in the interval [p, k].
func solve(p, k int, intervals [][2]int) (int, int) {
	events := make(map[int]int)
	events[p] += 0
	events[k+1] += 0
	for _, iv := range intervals {
		a, b := iv[0], iv[1]
		if b < p || a > k {
			continue
		}
		start := a
		if start < p {
			start = p
		}
		end := b
		if end > k {
			end = k
		}
		events[start]++
		events[end+1]--
	}
	times := make([]int, 0, len(events))
	for t := range events {
		times = append(times, t)
	}
	sort.Ints(times)
	cur := 0
	minCount := int(^uint(0) >> 1)
	maxCount := 0
	for i, t := range times {
		cur += events[t]
		nextTime := k + 1
		if i+1 < len(times) {
			nextTime = times[i+1]
		}
		if nextTime-1 < p || t > k {
			continue
		}
		if t < p {
			t = p
		}
		end := nextTime - 1
		if end > k {
			end = k
		}
		if t <= end {
			if cur < minCount {
				minCount = cur
			}
			if cur > maxCount {
				maxCount = cur
			}
		}
	}
	return minCount, maxCount
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for {
		var p, k int
		if _, err := fmt.Fscan(in, &p, &k); err != nil {
			break
		}
		var n int
		if _, err := fmt.Fscan(in, &n); err != nil {
			break
		}
		intervals := make([][2]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &intervals[i][0], &intervals[i][1])
		}
		mn, mx := solve(p, k, intervals)
		fmt.Fprintf(out, "%d %d\n", mn, mx)
	}
}
