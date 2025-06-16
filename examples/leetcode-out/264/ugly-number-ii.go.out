package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func min3(a int, b int, c int) int {
	var m int = a
	if (b < m) {
		m = b
	}
	if (c < m) {
		m = c
	}
	return m
}

func nthUglyNumber(n int) int {
	var i2 int = 0
	var i3 int = 0
	var i5 int = 0
	var uglies []int = []int{1}
	var count int = 1
	for (count < n) {
		var next2 int = (uglies[i2] * 2)
		var next3 int = (uglies[i3] * 3)
		var next5 int = (uglies[i5] * 5)
		var nextUgly int = min3(next2, next3, next5)
		uglies = append(append([]int{}, uglies...), []int{nextUgly}...)
		if (nextUgly == next2) {
			i2 = (i2 + 1)
		}
		if (nextUgly == next3) {
			i3 = (i3 + 1)
		}
		if (nextUgly == next5) {
			i5 = (i5 + 1)
		}
		count = (count + 1)
	}
	return uglies[(n - 1)]
}

func example_1() {
	expect((nthUglyNumber(10) == 12))
}

func example_2() {
	expect((nthUglyNumber(1) == 1))
}

func example_3() {
	expect((nthUglyNumber(3) == 3))
}

func larger_n() {
	expect((nthUglyNumber(15) == 24))
}

func main() {
	example_1()
	example_2()
	example_3()
	larger_n()
}

