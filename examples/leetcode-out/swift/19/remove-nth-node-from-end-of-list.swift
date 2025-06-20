import Foundation

func removeNthFromEnd(_ nums: [Int], _ n: Int) -> [Int] {
	let nums = nums
	let n = n
	
	let idx = nums.count - n
	var result: [Int] = []
	var i = 0
	while i < nums.count {
		if i != idx {
			result = result + [nums[i]]
		}
		i = i + 1
	}
	return result
}

func main() {
}
main()
