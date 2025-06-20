import Foundation

func swapPairs(_ nums: [Int]) -> [Int] {
	let nums = nums
	
	var i = 0
	var result: [Int] = []
	while i < nums.count {
		if i + 1 < nums.count {
			result = result + [nums[i + 1], nums[i]]
		} else {
			result = result + [nums[i]]
		}
		i = i + 2
	}
	return result
}

func main() {
}
main()
