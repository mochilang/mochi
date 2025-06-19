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

func digit(_ ch: String) -> Int {
	let ch = ch
	
	if ch == "0" {
		return 0
	}
	if ch == "1" {
		return 1
	}
	if ch == "2" {
		return 2
	}
	if ch == "3" {
		return 3
	}
	if ch == "4" {
		return 4
	}
	if ch == "5" {
		return 5
	}
	if ch == "6" {
		return 6
	}
	if ch == "7" {
		return 7
	}
	if ch == "8" {
		return 8
	}
	if ch == "9" {
		return 9
	}
	return -1
}

func myAtoi(_ s: String) -> Int {
	let s = s
	
	var i = 0
	let n = s.count
	while i < n && _indexString(s, i) == _indexString(" ", 0) {
		i = i + 1
	}
	var sign = 1
	if i < n && (_indexString(s, i) == _indexString("+", 0) || _indexString(s, i) == _indexString("-", 0)) {
		if _indexString(s, i) == _indexString("-", 0) {
			sign = -1
		}
		i = i + 1
	}
	var result = 0
	while i < n {
		let ch = _sliceString(s, i, i + 1)
		let d = digit(ch)
		if d < 0 {
			break
		}
		result = result * 10 + d
		i = i + 1
	}
	result = result * sign
	if result > 2147483647 {
		return 2147483647
	}
	if result < (-2147483648) {
		return -2147483648
	}
	return result
}

func main() {
}
main()
