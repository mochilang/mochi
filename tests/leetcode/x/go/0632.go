package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type item struct{ val, row, idx int }
type minHeap []item

func (h minHeap) Len() int            { return len(h) }
func (h minHeap) Less(i, j int) bool  { return h[i].val < h[j].val }
func (h minHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *minHeap) Push(x any)         { *h = append(*h, x.(item)) }
func (h *minHeap) Pop() any           { old := *h; x := old[len(old)-1]; *h = old[:len(old)-1]; return x }

func smallestRange(nums [][]int) (int, int) {
	h := &minHeap{}
	heap.Init(h)
	currentMax := -1 << 60
	for i, row := range nums {
		v := row[0]
		heap.Push(h, item{v, i, 0})
		if v > currentMax {
			currentMax = v
		}
	}
	bestL, bestR := (*h)[0].val, currentMax
	for {
		cur := heap.Pop(h).(item)
		if currentMax-cur.val < bestR-bestL || currentMax-cur.val == bestR-bestL && cur.val < bestL {
			bestL, bestR = cur.val, currentMax
		}
		if cur.idx+1 == len(nums[cur.row]) {
			return bestL, bestR
		}
		nv := nums[cur.row][cur.idx+1]
		heap.Push(h, item{nv, cur.row, cur.idx + 1})
		if nv > currentMax {
			currentMax = nv
		}
	}
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
		var k int
		fmt.Fscan(in, &k)
		nums := make([][]int, k)
		for i := 0; i < k; i++ {
			var n int
			fmt.Fscan(in, &n)
			nums[i] = make([]int, n)
			for j := 0; j < n; j++ {
				fmt.Fscan(in, &nums[i][j])
			}
		}
		l, r := smallestRange(nums)
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprintf(out, "[%d,%d]", l, r)
	}
}
