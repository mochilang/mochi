import Foundation

func removeDuplicates(_ nums: [Int]) -> Int {
	let nums = nums
	
	if nums.count == 0 {
		return 0
	}
	var count = 1
	var prev = nums[0]
	var i = 1
	while i < nums.count {
		let cur = nums[i]
		if cur != prev {
			count = count + 1
			prev = cur
		}
		i = i + 1
	}
	return count
}

func main() {
}
main()
