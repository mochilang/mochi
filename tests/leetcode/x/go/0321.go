package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func pick(nums []int, k int) []int {
	drop := len(nums) - k
	stack := []int{}
	for _, x := range nums {
		for drop > 0 && len(stack) > 0 && stack[len(stack)-1] < x {
			stack = stack[:len(stack)-1]
			drop--
		}
		stack = append(stack, x)
	}
	return append([]int{}, stack[:k]...)
}

func greater(a []int, i int, b []int, j int) bool {
	for i < len(a) && j < len(b) && a[i] == b[j] {
		i++
		j++
	}
	return j == len(b) || (i < len(a) && a[i] > b[j])
}

func merge(a, b []int) []int {
	out := make([]int, 0, len(a)+len(b))
	i, j := 0, 0
	for i < len(a) || j < len(b) {
		if greater(a, i, b, j) {
			out = append(out, a[i])
			i++
		} else {
			out = append(out, b[j])
			j++
		}
	}
	return out
}

func maxNumber(nums1, nums2 []int, k int) []int {
	best := []int{}
	start, end := 0, k
	if k > len(nums2) {
		start = k - len(nums2)
	}
	if end > len(nums1) {
		end = len(nums1)
	}
	for take := start; take <= end; take++ {
		cand := merge(pick(nums1, take), pick(nums2, k-take))
		if greater(cand, 0, best, 0) {
			best = cand
		}
	}
	return best
}

func fmtList(a []int) string {
	parts := make([]string, len(a))
	for i, v := range a {
		parts[i] = strconv.Itoa(v)
	}
	return "[" + strings.Join(parts, ",") + "]"
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	in.Split(bufio.ScanWords)
	nextInt := func() int {
		in.Scan()
		v, _ := strconv.Atoi(in.Text())
		return v
	}
	if !in.Scan() {
		return
	}
	t, _ := strconv.Atoi(in.Text())
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		n1 := nextInt()
		nums1 := make([]int, n1)
		for i := range nums1 {
			nums1[i] = nextInt()
		}
		n2 := nextInt()
		nums2 := make([]int, n2)
		for i := range nums2 {
			nums2[i] = nextInt()
		}
		k := nextInt()
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, fmtList(maxNumber(nums1, nums2, k)))
	}
}
