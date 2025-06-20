import Foundation

func threeSumClosest(_ nums: [Int], _ target: Int) -> Int {
	let nums = nums
	let target = target
	
	let sorted = nums.sorted()
	let n = sorted.count
	var best = sorted[0] + sorted[1] + sorted[2]
	for i in 0..<n {
		var left = i + 1
		var right = n - 1
		while left < right {
			let sum = sorted[i] + sorted[left] + sorted[right]
			if sum == target {
				return target
			}
			var diff = 0
			if sum > target {
				diff = sum - target
			} else {
				diff = target - sum
			}
			var bestDiff = 0
			if best > target {
				bestDiff = best - target
			} else {
				bestDiff = target - best
			}
			if diff < bestDiff {
				best = sum
			}
			if sum < target {
				left = left + 1
			} else {
				right = right - 1
			}
		}
	}
	return best
}

func main() {
}
main()
