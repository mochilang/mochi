package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"sort"
	"strconv"
)

type Pair struct{ h, r int }
type MaxHeap []Pair

func (h MaxHeap) Len() int            { return len(h) }
func (h MaxHeap) Less(i, j int) bool  { return h[i].h > h[j].h }
func (h MaxHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *MaxHeap) Push(x interface{}) { *h = append(*h, x.(Pair)) }
func (h *MaxHeap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[:n-1]
	return x
}

func solve(buildings [][]int) [][]int {
	events := make([][3]int, 0, len(buildings)*2)
	for _, b := range buildings {
		events = append(events, [3]int{b[0], -b[2], b[1]})
		events = append(events, [3]int{b[1], 0, 0})
	}
	sort.Slice(events, func(i, j int) bool {
		if events[i][0] != events[j][0] {
			return events[i][0] < events[j][0]
		}
		return events[i][1] < events[j][1]
	})
	h := &MaxHeap{{0, 1 << 60}}
	heap.Init(h)
	ans := [][]int{}
	for i := 0; i < len(events); {
		x := events[i][0]
		for i < len(events) && events[i][0] == x {
			if events[i][1] != 0 {
				heap.Push(h, Pair{-events[i][1], events[i][2]})
			}
			i++
		}
		for h.Len() > 0 && (*h)[0].r <= x {
			heap.Pop(h)
		}
		height := (*h)[0].h
		if len(ans) == 0 || ans[len(ans)-1][1] != height {
			ans = append(ans, []int{x, height})
		}
	}
	return ans
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	nextInt := func() int { sc.Scan(); v, _ := strconv.Atoi(sc.Text()); return v }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		n := nextInt()
		buildings := make([][]int, n)
		for i := 0; i < n; i++ {
			buildings[i] = []int{nextInt(), nextInt(), nextInt()}
		}
		ans := solve(buildings)
		if tc > 0 {
			fmt.Fprint(w, "\n\n")
		}
		fmt.Fprint(w, len(ans))
		for _, p := range ans {
			fmt.Fprintf(w, "\n%d %d", p[0], p[1])
		}
	}
}
