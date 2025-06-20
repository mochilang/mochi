import Foundation

func removeElement(_ nums: [Int], _ val: Int) -> Int {
	var nums = nums
	let val = val
	
	var k = 0
	var i = 0
	while i < nums.count {
		if nums[i] != val {
			nums[k] = nums[i]
			k = k + 1
		}
		i = i + 1
	}
	return k
}

func main() {
}
main()
