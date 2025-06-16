package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxArea(height []int) int {
	var left int = 0
	var right int = (len(height) - 1)
	var maxArea int = 0
	for (left < right) {
		var width int = (right - left)
		var h int = 0
		if (height[left] < height[right]) {
			h = height[left]
		} else {
			h = height[right]
		}
		var area int = (h * width)
		if (area > maxArea) {
			maxArea = area
		}
		if (height[left] < height[right]) {
			left = (left + 1)
		} else {
			right = (right - 1)
		}
	}
	return maxArea
}

func example_1() {
	expect((maxArea([]int{1, 8, 6, 2, 5, 4, 8, 3, 7}) == 49))
}

func example_2() {
	expect((maxArea([]int{1, 1}) == 1))
}

func decreasing_heights() {
	expect((maxArea([]int{4, 3, 2, 1, 4}) == 16))
}

func short_array() {
	expect((maxArea([]int{1, 2, 1}) == 2))
}

func main() {
	example_1()
	example_2()
	decreasing_heights()
	short_array()
}

