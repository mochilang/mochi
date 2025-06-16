package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func trap(height []int) int {
	var left int = 0
	var right int = (len(height) - 1)
	var left_max int = 0
	var right_max int = 0
	var water int = 0
	for (left < right) {
		if (height[left] < height[right]) {
			if (height[left] >= left_max) {
				left_max = height[left]
			} else {
				water = (water + ((left_max - height[left])))
			}
			left = (left + 1)
		} else {
			if (height[right] >= right_max) {
				right_max = height[right]
			} else {
				water = (water + ((right_max - height[right])))
			}
			right = (right - 1)
		}
	}
	return water
}

func example_1() {
	expect((trap([]int{0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1}) == 6))
}

func example_2() {
	expect((trap([]int{4, 2, 0, 3, 2, 5}) == 9))
}

func main() {
	example_1()
	example_2()
}

