package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type IntHeap struct {
	data []int
	less func(int, int) bool
}

func (h IntHeap) Len() int            { return len(h.data) }
func (h IntHeap) Less(i, j int) bool  { return h.less(h.data[i], h.data[j]) }
func (h IntHeap) Swap(i, j int)       { h.data[i], h.data[j] = h.data[j], h.data[i] }
func (h *IntHeap) Push(x any)         { h.data = append(h.data, x.(int)) }
func (h *IntHeap) Pop() any           { n := len(h.data); x := h.data[n-1]; h.data = h.data[:n-1]; return x }
func (h *IntHeap) Peek() int          { return h.data[0] }

type DualHeap struct {
	small, large       *IntHeap
	delayed            map[int]int
	k, smallSize, largeSize int
}

func NewDualHeap(k int) *DualHeap {
	small := &IntHeap{less: func(a, b int) bool { return a > b }}
	large := &IntHeap{less: func(a, b int) bool { return a < b }}
	heap.Init(small)
	heap.Init(large)
	return &DualHeap{small: small, large: large, delayed: map[int]int{}, k: k}
}

func (d *DualHeap) pruneSmall() {
	for d.small.Len() > 0 {
		x := d.small.Peek()
		if d.delayed[x] == 0 {
			break
		}
		d.delayed[x]--
		if d.delayed[x] == 0 {
			delete(d.delayed, x)
		}
		heap.Pop(d.small)
	}
}

func (d *DualHeap) pruneLarge() {
	for d.large.Len() > 0 {
		x := d.large.Peek()
		if d.delayed[x] == 0 {
			break
		}
		d.delayed[x]--
		if d.delayed[x] == 0 {
			delete(d.delayed, x)
		}
		heap.Pop(d.large)
	}
}

func (d *DualHeap) rebalance() {
	if d.smallSize > d.largeSize+1 {
		heap.Push(d.large, heap.Pop(d.small))
		d.smallSize--
		d.largeSize++
		d.pruneSmall()
	} else if d.smallSize < d.largeSize {
		heap.Push(d.small, heap.Pop(d.large))
		d.smallSize++
		d.largeSize--
		d.pruneLarge()
	}
}

func (d *DualHeap) add(x int) {
	if d.small.Len() == 0 || x <= d.small.Peek() {
		heap.Push(d.small, x)
		d.smallSize++
	} else {
		heap.Push(d.large, x)
		d.largeSize++
	}
	d.rebalance()
}

func (d *DualHeap) remove(x int) {
	d.delayed[x]++
	if x <= d.small.Peek() {
		d.smallSize--
		if x == d.small.Peek() {
			d.pruneSmall()
		}
	} else {
		d.largeSize--
		if d.large.Len() > 0 && x == d.large.Peek() {
			d.pruneLarge()
		}
	}
	d.rebalance()
}

func (d *DualHeap) median2() int64 {
	d.pruneSmall()
	d.pruneLarge()
	if d.k%2 == 1 {
		return int64(d.small.Peek()) * 2
	}
	return int64(d.small.Peek() + d.large.Peek())
}

func fmtVal(v2 int64) string {
	if v2%2 == 0 {
		return strconv.FormatInt(v2/2, 10)
	}
	sign := ""
	if v2 < 0 {
		sign = "-"
		v2 = -v2
	}
	return fmt.Sprintf("%s%d.5", sign, v2/2)
}

func solve(nums []int, k int) string {
	d := NewDualHeap(k)
	for i := 0; i < k; i++ {
		d.add(nums[i])
	}
	out := []string{fmtVal(d.median2())}
	for i := k; i < len(nums); i++ {
		d.add(nums[i])
		d.remove(nums[i-k])
		out = append(out, fmtVal(d.median2()))
	}
	return "[" + strings.Join(out, ",") + "]"
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var n, k int
		fmt.Fscan(in, &n, &k)
		nums := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &nums[i])
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, solve(nums, k))
	}
}
