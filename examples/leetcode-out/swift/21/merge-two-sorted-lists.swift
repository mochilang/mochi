import Foundation

func mergeTwoLists(_ l1: [Int], _ l2: [Int]) -> [Int] {
	let l1 = l1
	let l2 = l2
	
	var i = 0
	var j = 0
	var result: [Int] = []
	while i < l1.count && j < l2.count {
		if l1[i] <= l2[j] {
			result = result + [l1[i]]
			i = i + 1
		} else {
			result = result + [l2[j]]
			j = j + 1
		}
	}
	while i < l1.count {
		result = result + [l1[i]]
		i = i + 1
	}
	while j < l2.count {
		result = result + [l2[j]]
		j = j + 1
	}
	return result
}

func main() {
}
main()
