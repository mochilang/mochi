package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Project struct {
	capital int
	profit  int
}

type MaxHeap []int

func (h MaxHeap) Len() int           { return len(h) }
func (h MaxHeap) Less(i, j int) bool { return h[i] > h[j] }
func (h MaxHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }
func (h *MaxHeap) Push(x any)        { *h = append(*h, x.(int)) }
func (h *MaxHeap) Pop() any          { old := *h; x := old[len(old)-1]; *h = old[:len(old)-1]; return x }

func solve(k int, w int, profits []int, capital []int) int {
	n := len(profits)
	projects := make([]Project, n)
	for i := 0; i < n; i++ {
		projects[i] = Project{capital: capital[i], profit: profits[i]}
	}
	sort.Slice(projects, func(i, j int) bool {
		return projects[i].capital < projects[j].capital
	})
	h := &MaxHeap{}
	heap.Init(h)
	cur := w
	idx := 0
	for step := 0; step < k; step++ {
		for idx < n && projects[idx].capital <= cur {
			heap.Push(h, projects[idx].profit)
			idx++
		}
		if h.Len() == 0 {
			break
		}
		cur += heap.Pop(h).(int)
	}
	return cur
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for tc := 0; tc < t; tc++ {
		var n, k, w int
		fmt.Fscan(in, &n, &k, &w)
		profits := make([]int, n)
		capital := make([]int, n)
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &profits[i])
		}
		for i := 0; i < n; i++ {
			fmt.Fscan(in, &capital[i])
		}
		out = append(out, fmt.Sprint(solve(k, w, profits, capital)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
