package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isReflected(points [][]int) bool {
	if (len(points) == 0) {
		return true
	}
	var pointSet map[string]bool = map[string]bool{}
	var minX int = points[0][0]
	var maxX int = points[0][0]
	for _, p := range points {
		var x int = p[0]
		var y int = p[1]
		var key string = fmt.Sprint(x) + "," + fmt.Sprint(y)
		pointSet[key] = true
		if (x < minX) {
			minX = x
		}
		if (x > maxX) {
			maxX = x
		}
	}
	var sum int = (minX + maxX)
	for _, p := range points {
		var x int = p[0]
		var y int = p[1]
		var rx int = (sum - x)
		var key string = fmt.Sprint(rx) + "," + fmt.Sprint(y)
		_tmp0 := key
		_tmp1 := pointSet
		_, _tmp2 := _tmp1[_tmp0]
		if !(_tmp2) {
			return false
		}
	}
	return true
}

func example_true() {
	expect((isReflected([][]int{[]int{1, 1}, []int{-1, 1}}) == true))
}

func example_false() {
	expect((isReflected([][]int{[]int{1, 1}, []int{-1, -1}}) == false))
}

func three_points() {
	expect((isReflected([][]int{[]int{1, 1}, []int{0, 1}, []int{-1, 1}}) == true))
}

func duplicates() {
	expect((isReflected([][]int{[]int{0, 0}, []int{0, 0}}) == true))
}

func empty() {
	var pts [][]int = [][]int{}
	_ = pts
	expect((isReflected(pts) == true))
}

func main() {
	example_true()
	example_false()
	three_points()
	duplicates()
	empty()
}

