package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isRectangleCover(rectangles [][]int) bool {
	if (len(rectangles) == 0) {
		return false
	}
	var minX int = rectangles[0][0]
	var minY int = rectangles[0][1]
	var maxX int = rectangles[0][2]
	var maxY int = rectangles[0][3]
	var area int = 0
	var counts map[string]int = map[string]int{}
	for _, rect := range rectangles {
		var x1 int = rect[0]
		var y1 int = rect[1]
		var x2 int = rect[2]
		var y2 int = rect[3]
		if (x1 < minX) {
			minX = x1
		}
		if (y1 < minY) {
			minY = y1
		}
		if (x2 > maxX) {
			maxX = x2
		}
		if (y2 > maxY) {
			maxY = y2
		}
		area = (area + (((x2 - x1)) * ((y2 - y1))))
		var i int = 0
		var pts [][]int = [][]int{[]int{x1, y1}, []int{x1, y2}, []int{x2, y1}, []int{x2, y2}}
		for (i < 4) {
			var pt []int = pts[i]
			var key string = fmt.Sprint(pt[0]) + ":" + fmt.Sprint(pt[1])
			_tmp0 := key
			_tmp1 := counts
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				counts[key] = (counts[key] + 1)
			} else {
				counts[key] = 1
			}
			i = (i + 1)
		}
	}
	var expectArea int = (((maxX - minX)) * ((maxY - minY)))
	if (area != expectArea) {
		return false
	}
	var unique []string = []string{}
	for key := range counts {
		if ((counts[key] % 2) == 1) {
			unique = append(append([]string{}, unique...), []string{key}...)
		}
	}
	if (len(unique) != 4) {
		return false
	}
	var needed map[string]bool = map[string]bool{}
	needed[fmt.Sprint(minX) + ":" + fmt.Sprint(minY)] = true
	needed[fmt.Sprint(minX) + ":" + fmt.Sprint(maxY)] = true
	needed[fmt.Sprint(maxX) + ":" + fmt.Sprint(minY)] = true
	needed[fmt.Sprint(maxX) + ":" + fmt.Sprint(maxY)] = true
	for _, c := range unique {
		var ok bool = false
		_tmp3 := c
		_tmp4 := needed
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			ok = needed[c]
		}
		if !ok {
			return false
		}
	}
	return true
}

func example_1() {
	expect((isRectangleCover([][]int{[]int{1, 1, 3, 3}, []int{3, 1, 4, 2}, []int{3, 2, 4, 4}, []int{1, 3, 2, 4}, []int{2, 3, 3, 4}}) == true))
}

func example_2() {
	expect((isRectangleCover([][]int{[]int{1, 1, 2, 3}, []int{1, 3, 2, 4}, []int{3, 1, 4, 2}, []int{3, 2, 4, 4}}) == false))
}

func example_3() {
	expect((isRectangleCover([][]int{[]int{1, 1, 3, 3}, []int{3, 1, 4, 2}, []int{1, 3, 2, 4}, []int{3, 2, 4, 4}}) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
}

