package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxAreaAlt(height []int) int {
	var left int = 0
	var right int = (len(height) - 1)
	var best int = 0
	for (left < right) {
		var h int = height[left]
		if (height[right] < h) {
			h = height[right]
		}
		var area int = (h * ((right - left)))
		if (area > best) {
			best = area
		}
		if (height[left] < height[right]) {
			left = (left + 1)
		} else {
			right = (right - 1)
		}
	}
	return best
}

func example_1() {
	expect((maxAreaAlt([]int{1, 8, 6, 2, 5, 4, 8, 3, 7}) == 49))
}

func example_2() {
	expect((maxAreaAlt([]int{1, 1}) == 1))
}

func decreasing_heights() {
	expect((maxAreaAlt([]int{4, 3, 2, 1, 4}) == 16))
}

func short_array() {
	expect((maxAreaAlt([]int{1, 2, 1}) == 2))
}

func main() {
	example_1()
	example_2()
	decreasing_heights()
	short_array()
}

