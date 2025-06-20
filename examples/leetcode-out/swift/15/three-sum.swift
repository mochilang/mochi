import Foundation

func threeSum(_ nums: [Int]) -> [[Int]] {
	let nums = nums
	
	let sorted = nums.sorted()
	let n = sorted.count
	var res: [[Int]] = []
	var i = 0
	while i < n {
		if i > 0 && sorted[i] == sorted[i - 1] {
			i = i + 1
			continue
		}
		var left = i + 1
		var right = n - 1
		while left < right {
			let sum = sorted[i] + sorted[left] + sorted[right]
			if sum == 0 {
				res = res + [[sorted[i], sorted[left], sorted[right]]]
				left = left + 1
				while left < right && sorted[left] == sorted[left - 1] {
					left = left + 1
				}
				right = right - 1
				while left < right && sorted[right] == sorted[right + 1] {
					right = right - 1
				}
			} else 			if sum < 0 {
				left = left + 1
			} else {
				right = right - 1
			}
		}
		i = i + 1
	}
	return res
}

func main() {
}
main()
