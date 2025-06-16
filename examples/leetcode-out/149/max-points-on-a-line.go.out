package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxPoints(points [][]int) int {
	var n int = len(points)
	if (n <= 2) {
		return n
	}
	var answer int = 0
	var i int = 0
	for (i < n) {
		var slopes map[string]int = map[string]int{}
		var duplicates int = 1
		var j int = (i + 1)
		for (j < n) {
			var dx int = (points[j][0] - points[i][0])
			var dy int = (points[j][1] - points[i][1])
			if ((dx == 0) && (dy == 0)) {
				duplicates = (duplicates + 1)
			} else {
				var g int = gcd(dx, dy)
				var sx int = (dx / g)
				var sy int = (dy / g)
				if (sx == 0) {
					sy = 1
				} else 				if (sx < 0) {
					sx = -sx
					sy = -sy
				}
				var key string = fmt.Sprint(sx) + "/" + fmt.Sprint(sy)
				_tmp0 := key
				_tmp1 := slopes
				_, _tmp2 := _tmp1[_tmp0]
				if _tmp2 {
					slopes[key] = (slopes[key] + 1)
				} else {
					slopes[key] = 1
				}
			}
			j = (j + 1)
		}
		var localMax int = 0
		for key := range slopes {
			var count int = slopes[key]
			if (count > localMax) {
				localMax = count
			}
		}
		if ((localMax + duplicates) > answer) {
			answer = (localMax + duplicates)
		}
		i = (i + 1)
	}
	return answer
}

func abs(x int) int {
	if (x < 0) {
		return -x
	}
	return x
}

func gcd(a int, b int) int {
	var x int = abs(a)
	var y int = abs(b)
	for (y != 0) {
		var temp int = (x % y)
		x = y
		y = temp
	}
	return x
}

func example_1() {
	expect((maxPoints([][]int{[]int{1, 1}, []int{2, 2}, []int{3, 3}}) == 3))
}

func example_2() {
	expect((maxPoints([][]int{[]int{1, 1}, []int{3, 2}, []int{5, 3}, []int{4, 1}, []int{2, 3}, []int{1, 4}}) == 4))
}

func single_point() {
	expect((maxPoints([][]int{[]int{0, 0}}) == 1))
}

func main() {
	example_1()
	example_2()
	single_point()
}

