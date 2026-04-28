package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Course struct {
	duration int
	lastDay  int
}

type MaxHeap []int

func (h MaxHeap) Len() int           { return len(h) }
func (h MaxHeap) Less(i, j int) bool { return h[i] > h[j] }
func (h MaxHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }
func (h *MaxHeap) Push(x any)        { *h = append(*h, x.(int)) }
func (h *MaxHeap) Pop() any {
	old := *h
	x := old[len(old)-1]
	*h = old[:len(old)-1]
	return x
}

func solve(courses []Course) int {
	sort.Slice(courses, func(i, j int) bool {
		return courses[i].lastDay < courses[j].lastDay
	})
	total := 0
	h := &MaxHeap{}
	heap.Init(h)
	for _, c := range courses {
		total += c.duration
		heap.Push(h, c.duration)
		if total > c.lastDay {
			total -= heap.Pop(h).(int)
		}
	}
	return h.Len()
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := make([]string, 0, t)
	for i := 0; i < t; i++ {
		var n int
		fmt.Fscan(in, &n)
		courses := make([]Course, n)
		for j := 0; j < n; j++ {
			fmt.Fscan(in, &courses[j].duration, &courses[j].lastDay)
		}
		out = append(out, fmt.Sprint(solve(courses)))
	}
	fmt.Print(strings.Join(out, "\n\n"))
}
