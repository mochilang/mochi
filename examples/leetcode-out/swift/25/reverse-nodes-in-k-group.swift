import Foundation

func reverseKGroup(_ nums: [Int], _ k: Int) -> [Int] {
	let nums = nums
	let k = k
	
	let n = nums.count
	if k <= 1 {
		return nums
	}
	var result: [Int] = []
	var i = 0
	while i < n {
		let end = i + k
		if end <= n {
			var j = end - 1
			while j >= i {
				result = result + [nums[j]]
				j = j - 1
			}
		} else {
			var j = i
			while j < n {
				result = result + [nums[j]]
				j = j + 1
			}
		}
		i = i + k
	}
	return result
}

func main() {
}
main()
