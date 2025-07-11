//go:build ignore
// +build ignore

package main

import (
	"container/heap"
	"fmt"
	"math"
)

type point struct{ x, y int }

// priority queue item
type item struct{ s, a, b int }

type pq []item

func (p pq) Len() int            { return len(p) }
func (p pq) Less(i, j int) bool  { return p[i].s < p[j].s }
func (p pq) Swap(i, j int)       { p[i], p[j] = p[j], p[i] }
func (p *pq) Push(x interface{}) { *p = append(*p, x.(item)) }
func (p *pq) Pop() interface{}   { old := *p; n := len(old); x := old[n-1]; *p = old[:n-1]; return x }

// twosquares returns successive sets of integer vectors with equal squared length.
func twosquares() func() [][2]int {
	q := &pq{}
	n := 1
	return func() [][2]int {
		for q.Len() == 0 || n*n <= (*q)[0].s {
			heap.Push(q, item{s: n * n, a: n, b: 0})
			n++
		}
		s := (*q)[0].s
		var xy [][2]int
		for q.Len() > 0 && (*q)[0].s == s {
			it := heap.Pop(q).(item)
			xy = append(xy, [2]int{it.a, it.b})
			if it.a > it.b {
				heap.Push(q, item{s: it.a*it.a + (it.b+1)*(it.b+1), a: it.a, b: it.b + 1})
			}
		}
		return xy
	}
}

// genDirs yields direction vectors for the Babylonian spiral.
func genDirs(n int) []point {
	next := twosquares()
	dirs := make([]point, 0, n)
	d := [2]int{0, 1}
	for len(dirs) < n {
		v := next()
		list := append([][2]int{}, v...)
		temp := append([][2]int{}, list...)
		for _, ab := range temp {
			if ab[0] != ab[1] {
				list = append(list, [2]int{ab[1], ab[0]})
			}
		}
		temp = append([][2]int{}, list...)
		for _, ab := range temp {
			if ab[1] != 0 {
				list = append(list, [2]int{ab[0], -ab[1]})
			}
		}
		temp = append([][2]int{}, list...)
		for _, ab := range temp {
			if ab[0] != 0 {
				list = append(list, [2]int{-ab[0], ab[1]})
			}
		}
		bestDot := math.MinInt
		best := [2]int{}
		for _, ab := range list {
			if ab[0]*d[1]-ab[1]*d[0] >= 0 {
				dot := ab[0]*d[0] + ab[1]*d[1]
				if dot > bestDot {
					bestDot = dot
					best = ab
				}
			}
		}
		d = best
		dirs = append(dirs, point{d[0], d[1]})
	}
	return dirs
}

// positions returns the first n positions along the spiral.
func positions(n int) []point {
	dirs := genDirs(n)
	pos := make([]point, n)
	x, y := 0, 0
	for i, d := range dirs {
		pos[i] = point{x, y}
		x += d.x
		y += d.y
	}
	return pos
}

func main() {
	pts := positions(40)
	fmt.Println("The first 40 Babylonian spiral points are:")
	for i, p := range pts {
		fmt.Printf("%-10s", fmt.Sprintf("(%d, %d)", p.x, p.y))
		if (i+1)%10 == 0 {
			fmt.Println()
		}
	}
}
