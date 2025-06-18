import Foundation

func twoSum(_ nums: [Int], _ target: Int) -> [Int] {
	let nums = nums
	let target = target
	
	let n = nums.count
	for i in 0..<n {
		for j in i + 1..<n {
			if nums[i] + nums[j] == target {
				return [i, j]
			}
		}
	}
	return [-1, -1]
}

func main() {
	let result: [Int] = twoSum([2, 7, 11, 15], 9)
	print(result[0])
	print(result[1])
}
main()
