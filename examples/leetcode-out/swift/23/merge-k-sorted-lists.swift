import Foundation

func mergeKLists(_ lists: [[Int]]) -> [Int] {
	let lists = lists
	
	let k = lists.count
	var indices: [Int] = []
	var i = 0
	while i < k {
		indices = indices + [0]
		i = i + 1
	}
	var result: [Int] = []
	while true {
		var best = 0
		var bestList = -1
		var found = false
		var j = 0
		while j < k {
			let idx = indices[j]
			if idx < lists[j].count {
				let val = lists[j][idx]
				if !found || val < best {
					best = val
					bestList = j
					found = true
				}
			}
			j = j + 1
		}
		if !found {
			break
		}
		result = result + [best]
		indices[bestList] = indices[bestList] + 1
	}
	return result
}

func main() {
}
main()
