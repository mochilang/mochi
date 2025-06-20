import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func strStr(_ haystack: String, _ needle: String) -> Int {
	let haystack = haystack
	let needle = needle
	
	let n = haystack.count
	let m = needle.count
	if m == 0 {
		return 0
	}
	if m > n {
		return -1
	}
	for i in 0..<n - m + 1 {
		var j = 0
		while j < m {
			if _indexString(haystack, i + j) != _indexString(needle, j) {
				break
			}
			j = j + 1
		}
		if j == m {
			return i
		}
	}
	return -1
}

func main() {
}
main()
