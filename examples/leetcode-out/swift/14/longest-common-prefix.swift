import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {
	var start = i
	var end = j
	let chars = Array(s)
	let n = chars.count
	if start < 0 { start += n }
	if end < 0 { end += n }
	if start < 0 { start = 0 }
	if end > n { end = n }
	if end < start { end = start }
	return String(chars[start..<end])
}

func longestCommonPrefix(_ strs: [String]) -> String {
	let strs = strs
	
	if strs.count == 0 {
		return ""
	}
	var prefix = strs[0]
	for i in 1..<strs.count {
		var j = 0
		let current = strs[i]
		while j < prefix.count && j < current.count {
			if _indexString(prefix, j) != _indexString(current, j) {
				break
			}
			j = j + 1
		}
		prefix = _sliceString(prefix, 0, j)
		if prefix == "" {
			break
		}
	}
	return prefix
}

func main() {
}
main()
