import Foundation

func maxArea(_ height: [Int]) -> Int {
	let height = height
	
	var left = 0
	var right = height.count - 1
	var maxArea = 0
	while left < right {
		let width = right - left
		var h = 0
		if height[left] < height[right] {
			h = height[left]
		} else {
			h = height[right]
		}
		let area = h * width
		if area > maxArea {
			maxArea = area
		}
		if height[left] < height[right] {
			left = left + 1
		} else {
			right = right - 1
		}
	}
	return maxArea
}

func main() {
}
main()
