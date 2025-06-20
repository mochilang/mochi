import Foundation

func fourSum(_ nums: [Int], _ target: Int) -> [[Int]] {
	let nums = nums
	let target = target
	
	let sorted = nums.sorted()
	let n = sorted.count
	var result: [[Int]] = []
	for i in 0..<n {
		if i > 0 && sorted[i] == sorted[i - 1] {
			continue
		}
		for j in i + 1..<n {
			if j > i + 1 && sorted[j] == sorted[j - 1] {
				continue
			}
			var left = j + 1
			var right = n - 1
			while left < right {
				let sum = sorted[i] + sorted[j] + sorted[left] + sorted[right]
				if sum == target {
					result = result + [[sorted[i], sorted[j], sorted[left], sorted[right]]]
					left = left + 1
					right = right - 1
					while left < right && sorted[left] == sorted[left - 1] {
						left = left + 1
					}
					while left < right && sorted[right] == sorted[right + 1] {
						right = right - 1
					}
				} else 				if sum < target {
					left = left + 1
				} else {
					right = right - 1
				}
			}
		}
	}
	return result
}

func main() {
}
main()
