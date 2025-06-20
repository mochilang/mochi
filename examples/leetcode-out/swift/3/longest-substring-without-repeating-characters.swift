import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func lengthOfLongestSubstring(_ s: String) -> Int {
	let s = s
	
	let n = s.count
	var start = 0
	var best = 0
	var i = 0
	while i < n {
		var j = start
		while j < i {
			if _indexString(s, j) == _indexString(s, i) {
				start = j + 1
				break
			}
			j = j + 1
		}
		let length = i - start + 1
		if length > best {
			best = length
		}
		i = i + 1
	}
	return best
}

func main() {
}
main()
